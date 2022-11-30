module Game.Sxako.Cli.Brick (
  subCmdMain,
) where

import System.Environment
import System.Exit

{-
  TODO: this subcommand is an experimental TUI for UCI evaluation of a FEN or a live game.
 -}

subCmdMain :: String -> IO ()
subCmdMain cmdHelpPrefix =
  getArgs >>= \case
    _ -> do
      die $ cmdHelpPrefix <> "..."
