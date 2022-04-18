{-# LANGUAGE TypeApplications #-}

module Game.Sxako.Cli.Kbnk
  ( subCmdMain
  )
where

import Control.Monad.State.Strict
import qualified Data.Aeson as Aeson
import Data.List
import Game.Sxako.Bitboard
import Game.Sxako.Board
import Game.Sxako.Castling
import Game.Sxako.Common
import Game.Sxako.Coord
import Game.Sxako.Fen
import Game.Sxako.Ply
import Network.HTTP.Client
import Network.HTTP.Client.TLS (tlsManagerSettings)
import qualified Network.URI.Encode as URI
import System.Random

{-
  For KBNK (King + Bishop + Knight vs. King) endgame practice.

  - Generate a position:
    + pick 3 squares for white king, bishop, knight.
    + exclude all squares being attacked, place black king
      in one of the remaining squares.
      (this guarantees that black king is not in check).
    + 50% chance to flip color (and of course which side to move)
      just to avoid practicing as one specific side all the time.

  - Check with tablebase API to make sure we have a winnable position

  TODO:

  - Generate FEN and links for practice.

 -}

pick :: [a] -> [(a, [a])]
pick xs = map sp (init $ zip (inits xs) (tails xs))
  where
    sp (ls, v : rs) = (v, ls ++ rs)
    sp _ = error "cannot split empty list"

genBoard :: State StdGen Record
genBoard = do
  v0 <- state (\g -> uniformR (0, 63) g)
  let (whiteKing, cs0) = pick allCoords !! v0

  v1 <- state (\g -> uniformR (0, 62) g)
  let (whiteBishop, cs1) = pick cs0 !! v1

  v2 <- state (\g -> uniformR (0, 61) g)
  let (whiteKnight, cs2) = pick cs1 !! v2

  let bd0 =
        setBoardAt (White, Knight) whiteKnight True
          . setBoardAt (White, Bishop) whiteBishop True
          . setBoardAt (White, King) whiteKing True
          $ emptyBoard
      atk :: Bitboard
      atk = attackingSquares bd0 White
      blackKingCoords = filter (\c -> not (testBoard atk c)) cs2

  v3 <- state (\g -> uniformR (0, length blackKingCoords - 1) g)
  let (blackKing, _cs3) = pick blackKingCoords !! v3
      bd1 = setBoardAt (Black, King) blackKing True bd0
  shouldFlip <- state uniform
  pure $
    if shouldFlip
      then initRecord {placement = swapBoardSide bd1, activeColor = Black, castling = none}
      else initRecord {placement = bd1, activeColor = White, castling = none}

subCmdMain :: String -> IO ()
subCmdMain _cmdHelpPrefix = do
  g <- newStdGen
  let record = evalState genBoard g
  pprBoard (placement record)
  putStrLn $ "FEN: " <> encodeFen record

  mgr <- newManager tlsManagerSettings
  -- reference: https://github.com/lichess-org/lila-tablebase
  -- also: https://syzygy-tables.info/metrics regarding terms.
  let tableBaseApiLink = "http://tablebase.lichess.ovh/standard?fen=" <> URI.encode (encodeFen record)
  req <- parseRequest tableBaseApiLink
  resp <- httpLbs req mgr
  let raw = responseBody resp
  {-
    TODO: a bunch of things to be verified:

    - DTZ, DTM: not null, positive integer value, we need to extra those values
    - checkmate, stalemate, insufficient_material: false
    - category: win
   -}
  case Aeson.eitherDecode @Aeson.Value raw of
    Left msg -> error $ "Decode error: " <> msg
    Right v ->
      print v
