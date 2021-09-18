{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Game.Sxako.Cli.TestDataGen where

import Control.Applicative
import Control.Monad
import Data.Function
import Data.List
import Data.List.Split
import qualified Data.Map.Strict as M
import Data.Monoid
import qualified Data.Text as T
import Data.Yaml
import Game.Sxako.Fen
import Game.Sxako.Move
import System.Environment
import System.Exit
import System.IO
import System.Process.Typed
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

type PartialInfo = (Last Int, Last Ply)

consumePartialInfo :: [String] -> Either String (PartialInfo, [String])
consumePartialInfo = \case
  "depth" : _ : xs -> pure (mempty, xs)
  "seldepth" : _ : xs -> pure (mempty, xs)
  "time" : _ : xs -> pure (mempty, xs)
  "nodes" : _ : xs -> pure (mempty, xs)
  "pv" : raw : _xs | p <- read @Ply raw -> pure ((mempty, Last (Just p)), [])
  "multipv" : raw : xs | v <- read @Int raw -> pure ((Last (Just v), mempty), xs)
  "score" : _ : _ : xs -> pure (mempty, xs)
  "currmove" : _ : xs -> pure (mempty, xs)
  "currmovenumber" : _ : xs -> pure (mempty, xs)
  "hashfull" : _ : xs -> pure (mempty, xs)
  "nps" : _ : xs -> pure (mempty, xs)
  "tbhits" : _ : xs -> pure (mempty, xs)
  "cpuload" : _ : xs -> pure (mempty, xs)
  "string" : _ -> pure (mempty, [])
  "refutation" : _ -> pure (mempty, [])
  "currline" : _ -> pure (mempty, [])
  unknownTok : _ -> Left unknownTok
  _ -> pure (mempty, [])

parseInfo :: [String] -> Either String PartialInfo
parseInfo = \case
  [] -> pure mempty
  xs@(_ : _) -> do
    (r, ys) <- consumePartialInfo xs
    (r <>) <$> parseInfo ys

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
                    parser = (readS_to_P @Ply reads) `sepBy1` char ' '
            case (reads @Record fenRaw, parsePlies movesRaw) of
              ([(record, "")], Just ms) ->
                pure (pzId, record, ms)
              _ -> do
                putStrLn $ "Failed to parse FEN or moves."
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
                Just r -> do
                  putStrLn $ "> Ply: " <> show m
                  putStrLn $ "> FEN: " <> show r
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
        r' <- replayMoves record ms
        pure ()
      pure ()
    ["snapshot", inputFp] -> do
      Right tests <- decodeFileEither @[TestData] inputFp
      withStockfish $ \hIn hOut ->
        forM_ tests $ \td@TestData {tdPosition} -> do
          lps <- sfGetLegalPlies hIn hOut tdPosition
          pure td {tdLegalPlies = Just lps}
    _ -> do
      putStrLn $ cmdHelpPrefix <> "<testdata>"
      exitFailure

sfGetLegalPlies :: Handle -> Handle -> Record -> IO (M.Map Ply Record)
sfGetLegalPlies hIn hOut pos = do
  hPutStrLn hIn $ "position fen " <> show pos
  putStrLn $ "Sending position: " <> show pos
  hPutStrLn hIn "go depth 1"
  let collectInfo :: IO [[] String]
      collectInfo = do
        raw <- hGetLine hOut
        let ws = words raw
        if
            | "bestmove" `isPrefixOf` raw -> pure []
            | ["info", "string"] `isPrefixOf` ws -> collectInfo
            | ["info"] `isPrefixOf` ws -> (tail ws :) <$> collectInfo
            | otherwise -> collectInfo

      dbgParse :: [String] -> IO (Int, Ply)
      dbgParse xs = case parseInfo xs of
        Left msg -> do
          putStrLn $ "Parse failed for input: " <> show xs
          putStrLn $ "Reason: " <> msg
          exitFailure
        Right m@(Last ma, Last mb) ->
          case (,) <$> ma <*> mb of
            Nothing -> do
              putStrLn $ "Parse failed, only collected: " <> show m
              exitFailure
            Just v -> pure v
  sfLegalPlies <- collectInfo >>= mapM dbgParse
  sfPairs <- forM sfLegalPlies $ \(pvId, ply) -> do
    hPutStrLn hIn $ "position fen " <> show pos <> " moves " <> show ply
    hPutStrLn hIn "d"
    Just endFenRaw <-
      fix
        (\loop cur -> do
           raw <- hGetLine hOut
           let cur' =
                 cur <|> do
                   guard $ "Fen: " `isPrefixOf` raw
                   Just (drop 5 raw)
           -- very ugly way of checking end of the response, but I don't have anything better.
           if "Checkers: " `isPrefixOf` raw
             then pure cur'
             else loop cur')
        Nothing
    putStrLn $ "  multipv " <> show pvId <> ": " <> show ply
    let sfRecord = read @Record endFenRaw
    putStrLn $ "    sf result: " <> show sfRecord
    pure (ply, sfRecord)
  pure . M.fromList $ sfPairs

withStockfish :: (Handle -> Handle -> IO r) -> IO r
withStockfish f = do
  let pc =
        setStdout createPipe
          . setStdin createPipe
          $ proc "stockfish" []
  putStrLn "Starting communication with stockfish ..."
  withProcessWait pc $ \p -> do
    let hIn = getStdin p
        hOut = getStdout p
        collectOptions :: Maybe Int -> IO (Maybe Int)
        collectOptions m = do
          -- get UCI options, for now we are only interested in max of MultiPV.
          raw <- hGetLine hOut
          if
              | raw == "uciok" -> pure m
              | "option name MultiPV" `isPrefixOf` raw ->
                collectOptions
                  (m <|> (Just . read . last . words $ raw))
              | otherwise -> collectOptions m

    hSetBuffering hIn LineBuffering
    welcomeMsg <- hGetLine hOut
    putStrLn $ "Stockfish: " <> welcomeMsg
    hPutStrLn hIn "uci"
    Just c <- collectOptions Nothing
    putStrLn $ "MultiPV max value: " <> show c
    hPutStrLn hIn $ "setoption name MultiPV value " <> show c
    hPutStrLn hIn "isready"
    do
      raw <- hGetLine hOut
      guard $ raw == "readyok"
      putStrLn "Stockfish is ready."
    result <- f hIn hOut
    hPutStrLn hIn "quit"
    pure result
