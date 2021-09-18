{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Game.Sxako.Cli.TestDataGen where

import Control.Monad
import Data.List.Split
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import Data.Yaml
import Game.Sxako.Cli.Stockfish
import Game.Sxako.Fen
import Game.Sxako.Move
import System.Environment
import System.Exit
import Text.ParserCombinators.ReadP

{-
  Let's do all testdata in one single format:

  - tag: <any string>
  - position: <FEN string>
  - legal-plies (optional): when present, an object from long notation plies to new FEN string

  If we want to "follw some game", all we need is a sequence of plies
  and we can split then into such plie, applying one more ply at a time.

  TODO: since we are importing from Lichess data, might as well consume their CSV format.

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

subCmdMain :: String -> IO ()
subCmdMain cmdHelpPrefix =
  getArgs >>= \case
    ["from-lichess", inputFp] -> do
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
        TODO: read from CSV and generate YAML files:

        - tag: Lichess Puzzle {id}, {0}/{n}: init
        - tag: Lichess Puzzle {id}, {1}/{n}: {ply 1}
        - tag: Lichess Puzzle {id}, {2}/{n}: {ply 2}
        - ...
       -}
      puzzles <- mapM parseLine rawLines
      let replayMoves = foldM go
            where
              go record m = case legalPlies record M.!? m of
                Just r ->
                  pure r
                Nothing -> do
                  putStrLn "Verification failed:"
                  putStrLn $ "FEN: " <> show record
                  putStrLn $ "Move " <> show m <> " is not available."
                  exitFailure

      forM_ puzzles $ \(pzId, record, ms) -> do
        let recordFinalM = foldM go record ms
              where
                go recordCur m = do
                  r <- legalPlies recordCur M.!? m
                  pure r
        putStrLn $ "Puzzle: " <> pzId
        putStrLn $ "  FEN: " <> show record
        putStrLn $ "  Moves: " <> show ms
        putStrLn $ "  After: " <> show recordFinalM
        _r' <- replayMoves record ms
        pure ()
      pure ()
    ["snapshot", inputFp] -> do
      Right tests <- decodeFileEither @[TestData] inputFp
      withStockfish $ \sf ->
        forM_ tests $ \td@TestData {tdPosition} -> do
          lps <- getAllLegalPlies sf tdPosition
          pure td {tdLegalPlies = Just lps}
    _ -> do
      putStrLn $ cmdHelpPrefix <> "<testdata>"
      exitFailure
