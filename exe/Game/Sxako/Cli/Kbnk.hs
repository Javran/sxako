{-# LANGUAGE TypeApplications #-}

module Game.Sxako.Cli.Kbnk
  ( subCmdMain
  )
where

import Control.Monad.State.Strict
import Data.List
import Game.Sxako.Bitboard
import Game.Sxako.Board
import Game.Sxako.Common
import Game.Sxako.Coord
import Game.Sxako.Ply
import System.Random

{-
  For KBNK (King + Bishop + Knight vs. King) endgame practice.

  TODO:

  - Generate a position:
    + pick 3 squares for white king, bishop, knight.
    + exclude all squares being attacked, place black king
      in one of the remaining squares.
    + 50% chance to flip color (and of course which side to move)
      just to avoid practicing as one specific side all the time.

  - Preliminary validation
    (simple validations like white king should not be in check
    when it's black to move)
  - Check with tablebase API to make sure we have a winnable position
  - Generate FEN and links for practice.

 -}

pick :: [a] -> [(a, [a])]
pick xs = map sp (init $ zip (inits xs) (tails xs))
  where
    sp (ls, v : rs) = (v, ls ++ rs)
    sp _ = error "cannot split empty list"

genBoard :: State StdGen Board
genBoard = do
  v0 <- state (\g -> uniformR (0, 63) g)
  let (whiteKing, cs0) = pick allCoords !! v0

  v1 <- state (\g -> uniformR (0, 62) g)
  let (whiteBishop, cs1) = pick cs0 !! v1

  v2 <- state (\g -> uniformR (0, 61) g)
  let (whiteKnight, cs2) = pick cs1 !! v2

  let bd =
        setBoardAt (White, Knight) whiteKnight True
          . setBoardAt (White, Bishop) whiteBishop True
          . setBoardAt (White, King) whiteKing True
          $ emptyBoard
      atk :: Bitboard
      atk = attackingSquares bd White
      blackKingCoords = filter (\c -> not (testBoard atk c)) cs2

  v3 <- state (\g -> uniformR (0, length blackKingCoords - 1) g)
  let (blackKing, _cs3)  = pick blackKingCoords !! v3
  pure $ setBoardAt (Black, King) blackKing True bd

subCmdMain :: String -> IO ()
subCmdMain _cmdHelpPrefix = do
  g <- newStdGen
  let bd = evalState genBoard g
  pprBoard bd
  print bd
  -- TODO
  pure ()
