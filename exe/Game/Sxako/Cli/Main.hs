{-# LANGUAGE LambdaCase #-}

module Game.Sxako.Cli.Main
  ( main
  )
where

import Control.Monad
import qualified Game.Sxako.Cli.Render as Render
import qualified Game.Sxako.Cli.TestDataGen as TestDataGen
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
      ]
