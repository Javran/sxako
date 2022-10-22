{-# LANGUAGE LambdaCase #-}

module Game.Sxako.Cli.Main
  ( main
  )
where

import Control.Monad
import qualified Game.Sxako.Cli.Kbnk as Kbnk
import qualified Game.Sxako.Cli.ParsePgn as ParsePgn
import qualified Game.Sxako.Cli.Render as Render
import qualified Game.Sxako.Cli.TestDataGen as TestDataGen
import qualified Game.Sxako.Cli.Dev as Dev
import System.Environment
import System.Exit

main :: IO ()
main =
  getArgs >>= \case
    subCmd : args
      | Just handler <- lookup subCmd handlers ->
        withArgs args (handler ("<prog> " <> subCmd <> " "))
    _ -> do
      forM_ handlers $ \(sub, _) ->
        putStrLn $ "<prog> " <> sub <> " ..."
      exitFailure
  where
    handlers =
      [ ("render", Render.subCmdMain)
      , ("testdata-gen", TestDataGen.subCmdMain)
      , ("parse-pgn", ParsePgn.subCmdMain)
      , ("kbnk", Kbnk.subCmdMain)
      , ("dev", Dev.subCmdMain)
      ]
