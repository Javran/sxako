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
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import Data.Yaml
import Game.Sxako.Fen
import Game.Sxako.Move
import System.Environment
import System.Exit
import System.IO
import System.Process.Typed

data TestPayload
  = -- | all legal moves and their resulting positions should match.
    LegalPlies (Maybe (M.Map Ply Record))
  | -- | follow along a game and expect resulting positions to match
    Follow (Maybe [(Ply, Record)])
  deriving (Show)

data TestData = TestData
  { tdTag :: T.Text
  , tdPosition :: Record
  , tdPayload :: TestPayload
  }
  deriving (Show)

instance FromJSON TestData where
  parseJSON = withObject "TestData" $ \o -> do
    tdTag <- o .: "tag"
    tdPosition <- o .: "position"
    mode <- o .: "mode"
    tdPayload <-
      if
          | mode == "legal-plies" -> pure (LegalPlies Nothing)
          | mode == "follow" -> pure (Follow Nothing)
          | otherwise -> fail $ "Unknown mode: " <> mode
    pure $ TestData {tdTag, tdPosition, tdPayload}

subCmdMain :: String -> IO ()
subCmdMain cmdHelpPrefix =
  getArgs >>= \case
    [inputFp] -> do
      Right tests <- decodeFileEither @[TestData] inputFp
      withStockfish $ \hIn hOut ->
        forM_ tests $ \TestData {tdPosition} -> do
          hPutStrLn hIn $ "position fen " <> show tdPosition
          hPutStrLn hIn "go depth 1"
          fix $ \loop -> do
            raw <- hGetLine hOut
            if "bestmove" `isPrefixOf` raw
              then pure ()
              else putStrLn raw >> loop
          pure ()
    _ -> do
      putStrLn $ cmdHelpPrefix <> "<testdata>"
      exitFailure

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
