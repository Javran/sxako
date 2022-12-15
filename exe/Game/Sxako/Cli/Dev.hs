module Game.Sxako.Cli.Dev (
  subCmdMain,
) where

import Control.Monad
import qualified Data.Map.Strict as M
import Game.Sxako.Board
import Game.Sxako.Fen
import Game.Sxako.Ply
import Game.Sxako.San
import qualified Game.Sxako.Pgn.Pass1 as P1
import System.Environment
import System.Exit
import Shower

{-
  TODO: interpret stockfish plies (long notation).

  An example taken from:  https://lichess.org/study/URRD3KP3

  > uciok
  > position fen 8/6pp/1b6/7p/6pr/2N1Pprp/3K1ppn/R4bqk w - - 0 1
  > go mate 27

  Stockfish finds:

  > info depth 60 seldepth 60 multipv 1 score mate 27 nodes 2857836 nps 3056509 hashfull 339 tbhits 0 time 935 pv c3e4 b6c7 a1d1 c7e5 d2c1 e5c7 c1b2 c7e5 b2a3 e5c7 a3b4 c7b8 b4c5 b8e5 d1c1 e5b8 c1a1 b8c7 a1b1 c7e5 b1d1 g7g6 d1c1 e5b8 c1a1 b8c7 a1b1 c7e5 b1d1 h7h6 d1c1 e5b8 c1a1 b8c7 a1b1 c7e5 b1d1 g6g5 d1a1 e5b8 a1c1 b8c7 c1b1 c7e5 b1d1 e5b8 d1d6 b8a7 c5b4 a7c5 b4c5 f1e2 e4g3

  Pending test coverage:
  - en passant
  - long / short castling
  - promotion

 -}

subCmdMain :: String -> IO ()
subCmdMain cmdHelpPrefix =
  getArgs >>= \case
    ["go"] -> do
      let r = read @Record "8/6pp/1b6/7p/6pr/2N1Pprp/3K1ppn/R4bqk w - - 0 1"
          plies = fmap (read @Ply) $ words "c3e4 b6c7 a1d1 c7e5 d2c1 e5c7 c1b2 c7e5 b2a3 e5c7 a3b4 c7b8 b4c5 b8e5 d1c1 e5b8 c1a1 b8c7 a1b1 c7e5 b1d1 g7g6 d1c1 e5b8 c1a1 b8c7 a1b1 c7e5 b1d1 h7h6 d1c1 e5b8 c1a1 b8c7 a1b1 c7e5 b1d1 g6g5 d1a1 e5b8 a1c1 b8c7 c1b1 c7e5 b1d1 e5b8 d1d6 b8a7 c5b4 a7c5 b4c5 f1e2 e4g3"

      fin <-
        foldM
          ( \cur ply -> do
              let (pm, (pToS, _)) = legalPliesWithMapping cur
              case pm M.!? ply of
                Just next -> do
                  print (ply, pToS M.! ply, next)
                  pure next
                Nothing -> die $ "invalid ply: " <> show (cur, ply)
          )
          r
          plies
      pprBoard (placement fin)
    ["pgn"] -> do
      printer (P1.parse P1.example0)
    _ -> do
      die $ cmdHelpPrefix <> "..."
