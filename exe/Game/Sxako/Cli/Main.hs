module Game.Sxako.Cli.Main (
  main,
) where

import Control.Monad
import qualified Game.Sxako.Cli.Dev as Dev
import qualified Game.Sxako.Cli.Kbnk as Kbnk
import qualified Game.Sxako.Cli.ParsePgn as ParsePgn
import qualified Game.Sxako.Cli.Render as Render
import qualified Game.Sxako.Cli.TestDataGen as TestDataGen
import qualified Game.Sxako.Cli.Brick as Brick
import System.Environment
import System.Exit

{- Copy this to new sub-command module as a starter kit. -}
_subCmdMain :: String -> IO ()
_subCmdMain cmdHelpPrefix =
  getArgs >>= \case
    _ -> do
      die $ cmdHelpPrefix <> "..."

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
      , ("brick", Brick.subCmdMain)
      , ("dev", Dev.subCmdMain)
      ]
