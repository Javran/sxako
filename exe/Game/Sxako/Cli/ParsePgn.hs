module Game.Sxako.Cli.ParsePgn (
  subCmdMain,
) where

import Data.Attoparsec.ByteString.Char8 as Parser
import qualified Data.ByteString as BS
import Game.Sxako.Pgn
import System.Environment
import System.Exit
import Text.Printf

subCmdMain :: String -> IO ()
subCmdMain cmdHelpPrefix =
  getArgs >>= \case
    [pgnFp] -> do
      raw <- BS.readFile pgnFp
      let parser = do
            -- fuck BOM
            _ <- option () (() <$ string "\239\187\191")
            ps <- manyPgnsP
            leftover <- manyTill' anyChar endOfInput
            pure (ps, leftover)
      case Parser.parseOnly parser raw of
        Left msg ->
          die $ "Failed when parsing: " <> msg
        Right (r, leftover) ->
          if null leftover
            then do
              mapM_ (\l -> print l >> putStrLn "") r
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
    _ -> do
      putStrLn $ cmdHelpPrefix <> "<PGN file>"
      exitFailure
