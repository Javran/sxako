{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Game.Sxako.Cli.Stockfish
  ( SfHandle
  , withStockfish
  , getAllLegalPlies
  )
where

{-
  Communication with stockfish through UCI protocl.

  Reference: http://wbec-ridderkerk.nl/html/UCIProtocol.html

 -}
import Control.Applicative
import Control.Monad
import Data.Function
import Data.List
import qualified Data.Map.Strict as M
import Data.Monoid
import Game.Sxako.Fen
import Game.Sxako.Move
import System.Exit
import System.IO
import System.Process.Typed

data SfHandle = SfHandle Handle Handle

withStockfish :: (SfHandle -> IO r) -> IO r
withStockfish f = do
  let pc =
        setStdout createPipe
          . setStdin createPipe
          $ proc "stockfish" []
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
    _welcomeMsg <- hGetLine hOut
    hPutStrLn hIn "uci"
    Just c <- collectOptions Nothing
    hPutStrLn hIn $ "setoption name MultiPV value " <> show c
    hPutStrLn hIn "isready"
    do
      raw <- hGetLine hOut
      guard $ raw == "readyok"
      putStrLn "Stockfish is ready."
    result <- f (SfHandle hIn hOut)
    hPutStrLn hIn "quit"
    pure result

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

getAllLegalPlies :: SfHandle -> Record -> IO (M.Map Ply Record)
getAllLegalPlies (SfHandle hIn hOut) pos = do
  hPutStrLn hIn $ "position fen " <> show pos
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
  sfPairs <- forM sfLegalPlies $ \(_pvId, ply) -> do
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
    let sfRecord = read @Record endFenRaw
    pure (ply, sfRecord)
  pure . M.fromList $ sfPairs
