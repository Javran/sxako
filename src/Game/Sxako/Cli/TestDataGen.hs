{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Game.Sxako.Cli.TestDataGen where

import Control.Monad
import Data.Aeson
import Data.List.Split
import qualified Data.Map.Strict as M
import Data.Maybe
import qualified Data.Text as T
import Data.Text.Encoding
import Data.Yaml
import Game.Sxako.Cli.Stockfish
import Game.Sxako.Fen
import Game.Sxako.Move
import System.Environment
import System.Exit
import System.IO
import Text.ParserCombinators.ReadP

{-
  Note on generating testdata:

  We are making test cases with Lichess puzzles as input:

  For each puzzle, we follow along the move list and snapshot all legal moves
  as we proceed and those snapshots are recorded as testdata.

  It is worth noting that we are actually generating testdata
  while verifying it against our own implementation.
  This is because UCI protocol does not have a good way of reporting illegal moves,
  so we have to be careful with each step to ensure that both UCI engine
  and our implementation are fed with the same input FEN.

  The fact that testdata generation does the actual test is a bit unfortunate,
  but I can't think of any simple and better way at the moment.

 -}

{-
  Let's do all testdata in one single format:

  - tag: <any string>
  - position: <FEN string>
  - legal-plies (optional): when present, an object from long notation plies to new FEN string

  If we want to "follw some game", all we need is a sequence of plies
  and we can split then into such plie, applying one more ply at a time.

 -}
data TestData = TestData
  { tdTag :: T.Text
  , tdPosition :: Record
  , tdLegalPlies :: Maybe (M.Map Ply Record)
  }
  deriving (Show)

instance FromJSON TestData where
  parseJSON = withObject "TestData" $ \o -> do
    tdTag <- o .: "tag"
    tdPosition <- o .: "position"
    tdLegalPlies <- o .:? "legal-plies"
    pure $ TestData {tdTag, tdPosition, tdLegalPlies}

instance ToJSON TestData where
  toJSON TestData {tdTag, tdPosition, tdLegalPlies} =
    object $
      [ "tag" .= tdTag
      , "position" .= tdPosition
      ]
        <> maybe [] ((: []) . ("legal-plies" .=)) tdLegalPlies

  toEncoding TestData {tdTag, tdPosition, tdLegalPlies} =
    pairs
      ("tag" .= tdTag
         <> "position" .= tdPosition
         <> maybe mempty ("legal-plies" .=) tdLegalPlies)

type LegalMoves = M.Map Ply Record

{-
  Turn a puzzle into a sequence of snapshots.
 -}
snapshotPuzzle :: SfProcess -> Record -> [Ply] -> IO [(Record, LegalMoves)]
snapshotPuzzle sf r ps = do
  lps <- getAllLegalPlies sf r
  ((r, lps) :) <$> case ps of
    [] -> pure []
    ply : xs -> do
      let myNextPos = legalPliesMap r M.!? ply
          sfNextPos = lps M.!? ply
      nextPos <- case sfNextPos of
        Nothing -> do
          putStrLn $ "Illegal ply (stockfish): " <> show (r, ply)
          exitFailure
        Just np -> do
          unless (myNextPos == sfNextPos) $ do
            putStrLn "stockfish and sxako do not agree on next position."
            putStrLn $ "State: " <> show (r, ply)
            putStrLn "stockfish:"
            print sfNextPos
            putStrLn "sxako:"
            print myNextPos
            exitFailure
          pure np
      snapshotPuzzle sf nextPos xs

subCmdMain :: String -> IO ()
subCmdMain cmdHelpPrefix =
  getArgs >>= \case
    "from-lichess" : inputFp : mOutputFp
      | -- zero or one element.
        take 1 mOutputFp == mOutputFp ->
        do
          rawLines <- lines <$> readFile inputFp
          let parseLine raw = do
                let xs = splitOn "," raw
                    actualLen = length xs
                unless (actualLen == 9) $ do
                  putStrLn $ "Cannot parse:" <> raw
                  putStrLn $ "Expected 9 elements but " <> show actualLen <> " were found."
                  exitFailure
                let pzId : fenRaw : movesRaw : _ = xs
                    parsePlies ms = case readP_to_S (parser <* eof) ms of
                      [(v, "")] -> pure v
                      _ -> Nothing
                      where
                        parser = readS_to_P @Ply reads `sepBy1` char ' '
                case (reads @Record fenRaw, parsePlies movesRaw) of
                  ([(record, "")], Just ms) ->
                    pure (pzId, record, ms)
                  _ -> do
                    putStrLn "Failed to parse FEN or moves."
                    putStrLn $ "Raw FEN: " <> fenRaw
                    putStrLn $ "Raw moves: " <> movesRaw
                    exitFailure

          {-
            read from CSV and generate YAML files.
           -}
          puzzles <- mapM parseLine rawLines
          tds <-
            concat
              <$> withStockfish
                (\sf ->
                   forM (zip [0 :: Int ..] puzzles) $ \(ind, (pzId, record, ms)) -> do
                     let prefix = "Lichess Puzzle #" <> pzId <> ", "
                         l = length ms
                         tags = "init" : zipWith (\i p -> show i <> "/" <> show l <> ": " <> show p) [1 :: Int ..] ms
                     rs <- snapshotPuzzle sf record ms
                     when (ind `rem` 100 == 0) $
                       hPutStrLn stderr $ "Processing " <> show (ind + 1) <> " of " <> show (length puzzles) <> " ..."
                     forM (zip tags rs) $ \(tag, (tdPosition, lps)) -> do
                       let tdTag = T.pack $ prefix <> tag
                       pure TestData {tdTag, tdPosition, tdLegalPlies = Just lps})
          case listToMaybe mOutputFp of
            Just outputFp -> do
              Data.Yaml.encodeFile outputFp tds
              putStrLn $ show (length tds) <> " testdata written to " <> outputFp
            Nothing -> do
              putStrLn "# BEGIN"
              putStr (T.unpack (decodeUtf8 (Data.Yaml.encode tds)))
              putStrLn "# END"
    ["snapshot", inputFp] -> do
      Right tests <- decodeFileEither @[TestData] inputFp
      putStrLn $ "Found " <> show (length tests) <> " tests."
      withStockfish $ \sf ->
        forM_ tests $ \td@TestData {tdPosition} -> do
          lps <- getAllLegalPlies sf tdPosition
          pure td {tdLegalPlies = Just lps}
    _ -> do
      putStrLn $ cmdHelpPrefix <> "from-lichess <source csv> [target]"
      putStrLn $ cmdHelpPrefix <> "snapshot <test yaml>"
      exitFailure
