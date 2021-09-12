{-# LANGUAGE ScopedTypeVariables #-}

module Game.Sxako.Cli.Stockfish where

{-
  Communication with stockfish through UCI protocl.

  Reference: http://wbec-ridderkerk.nl/html/UCIProtocol.html

 -}

import Data.Function
import Data.List
import System.IO
import System.Process.Typed

subCmdMain :: String -> IO ()
subCmdMain _cmdHelpPrefix = do
  let pc =
        setStdout createPipe
          . setStdin createPipe
          $ proc "stockfish" []
  putStrLn "Start communication with stockfish:"
  c <- withProcessWait pc $ \p -> do
    let hIn = getStdin p
        hOut = getStdout p
        consumePut = hGetLine hOut >>= print
    hSetBuffering hIn LineBuffering
    consumePut
    hPutStrLn hIn "uci"
    fix $ \loop -> do
      raw <- hGetLine hOut
      case raw of
        "uciok" -> pure ()
        _ -> putStrLn raw >> loop
    hPutStrLn hIn "d"
    fix $ \loop -> do
      raw <- hGetLine hOut
      putStrLn raw
      -- very ugly way of checking end of the response, but I don't have anything better.
      if "Checkers: " `isPrefixOf` raw
        then pure ()
        else loop
    hPutStrLn hIn "quit"
  putStrLn $ "Process terminated: " <> show c
