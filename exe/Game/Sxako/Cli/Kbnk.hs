module Game.Sxako.Cli.Kbnk
  ( subCmdMain
  )
where

{-
  For KBNK (King + Bishop + Knight vs. King) endgame practice.

  TODO:

  - Generate a position with preliminary validation
    (simple validations like white king should not be in check
    when it's black to move)
  - Check with tablebase API to make sure we have a winnable position
  - Generate FEN and links for practice.

 -}

subCmdMain :: String -> IO ()
subCmdMain _cmdHelpPrefix = do
  -- TODO
  pure ()
