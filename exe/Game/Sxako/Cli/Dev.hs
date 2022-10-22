module Game.Sxako.Cli.Dev (
  subCmdMain,
) where

import System.Environment
import System.Exit

subCmdMain :: String -> IO ()
subCmdMain cmdHelpPrefix =
  getArgs >>= \case
    _ -> do
      die $ cmdHelpPrefix <> "..."
