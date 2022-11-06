module Game.Sxako.Cli.ParsePgn (
  subCmdMain,
) where

import Control.DeepSeq
import Control.Exception (evaluate)
import Control.Monad
import Data.Attoparsec.ByteString.Char8 as Parser
import qualified Data.ByteString as BS
import Game.Sxako.Pgn
import System.Environment
import System.Exit
import System.TimeIt (timeItT)
import Text.Printf

parseExperiment :: FilePath -> IO ()
parseExperiment pgnFp = do
  raw <- BS.readFile pgnFp
  let parser = do
        -- fuck BOM
        _ <- option () (void (string "\239\187\191"))
        ps <- manyPgnsP
        leftover <- manyTill' anyChar endOfInput
        pure (ps, leftover)
  -- forcing input.
  raw' <- evaluate $!! raw
  -- forcing output.
  (t, parsed) <- timeItT $ evaluate $!! Parser.parseOnly parser raw'
  printf "Took %f seconds (CPU time)\n" t
  case parsed of
    Left msg ->
      die $ "Failed when parsing: " <> msg
    Right (r, leftover) ->
      if null leftover
        then do
          printf "Parsed %d records, all succeeded.\n" (length r)
        else do
          printf
            "Parsed %d records, with leftover length %d.\n"
            (length r)
            (length leftover)
          case r of
            [] -> pure ()
            _ -> do
              putStrLn "Last record:"
              print $ last r
          putStrLn "Start of leftover:"
          putStrLn $ Prelude.take 100 leftover

subCmdMain :: String -> IO ()
subCmdMain cmdHelpPrefix =
  getArgs >>= \case
    pgnFps | () == () -> do
      forM_ pgnFps \pgnFp -> do
        putStrLn $ "Processing " <> pgnFp <> " ..."
        parseExperiment pgnFp
    _ -> do
      putStrLn $ cmdHelpPrefix <> "<PGN file>"
      exitFailure
