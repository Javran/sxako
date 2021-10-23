{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

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
      let fuckBom = option () (() <$ string "\239\187\191")
      case Parser.parseOnly (fuckBom *> manyPgnsP <* endOfInput) raw of
        Left msg -> putStrLn msg
        Right r -> mapM_ (\l -> print l >> putStrLn "") r
    _ -> do
      putStrLn $ cmdHelpPrefix <> "<PGN file>"
      exitFailure
