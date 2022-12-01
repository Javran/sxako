module Game.Sxako.Cli.Brick (
  subCmdMain,
) where

import Game.Sxako.Cli.Brick.Brick as B
import System.Environment

{-
  TODO: this subcommand is an experimental TUI for UCI evaluation of a FEN or a live game.
 -}

subCmdMain :: String -> IO ()
subCmdMain _cmdHelpPrefix = getArgs >>= B.mainWithArgs
