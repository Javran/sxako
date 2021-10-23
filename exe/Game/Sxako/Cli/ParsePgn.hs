{-# LANGUAGE LambdaCase #-}

module Game.Sxako.Cli.ParsePgn
  ( subCmdMain
  )
where

import Data.Attoparsec.ByteString.Char8 as Parser
import qualified Data.ByteString as BS
import Game.Sxako.Pgn
import System.Environment
import System.Exit

subCmdMain :: String -> IO ()
subCmdMain cmdHelpPrefix =
  getArgs >>= \case
    [pgnFp] -> do
      raw <- BS.readFile pgnFp
      case Parser.parseOnly manyPgnsP raw of
        Left msg -> putStrLn msg
        Right r -> print r
    _ -> do
      putStrLn $ cmdHelpPrefix <> "<PGN file>"
      exitFailure
