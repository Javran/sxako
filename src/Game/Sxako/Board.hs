module Game.Sxako.Board (
  Board,
  Halfboard,
  fromPlacement2d,
  emptyHb,
  hbAt,
  at,
  infoOccupied,
  getHalfboard,
  pprBoard,
  unpackToFenOrd,
  setBoardAt,
  emptyBoard,
  swapBoardSide,
) where

import Control.Monad
import Control.Monad.ST.Strict
import Data.Bits
import Data.Foldable
import Data.Function
import Data.List
import qualified Data.List.NonEmpty as NE
import Data.Maybe
import Game.Sxako.Bitboard
import qualified Game.Sxako.Board.Halfboard as Hb
import Game.Sxako.Common
import Game.Sxako.Coord

{-
  TODO: implement an alternative Board representation using standard vector.
  profiling indicates that the slowness mostly come from fixed-vector operations -
  probably Peano-encoding makes accesses not really constant but linear.
 -}

{-
  There are multiple ways to represent a board with Bitboard as elements:

  - we can do a vector of 12 elements
  - or a pair of vectors of 6 elements in each

  And when it comes to the choice of boxed / unboxed vector, we can do both.

  For now performance doesn't matter, so let's just pick one and stick with it
  unless there are blocking problems along the way.

  I choose to use a pair of boxed vectors so to allow more sharing.
 -}

{-
  TODO: we can make board representation more compact.
  This idea is inspired from Stockfish source code:

  We will have 7 bitboards for:
  1. "white" board, marking all squares occupied by white's pieces (including pawns)
  2. "black" board, similar to "white" board
  3. "king" board, marking all squares occupied by kings regardless of color
  4. "queen" board, similar
  5. "rook" board, similar
  6. "bishop" board, similar
  7. "knight" board, similar

  Note the lack of a "pawn" board - this bit of info can be inferred.

  Note that we can even eliminate "queen" board by recording queen's locations
  in both "rook" board and "bishop" board.
 -}

-- type Halfboard = Vec FixedVector

{-
  (<white side>, <black side>)
 -}
newtype Board = Board (Halfboard, Halfboard) deriving (Eq, Ord)

type Halfboard = Hb.Halfboard

fromPlacement2d :: Placement2D -> Board
fromPlacement2d ps2d = runST $ do
  whiteHb <- Hb.thaw Hb.empty
  blackHb <- Hb.thaw Hb.empty
  let pairs :: [] (Coord, Piece)
      pairs =
        catMaybes
          ( zipWith
              (\mcp c -> (c,) <$> mcp)
              (concat $ toList $ fmap toList ps2d)
              (concat fenCoords)
          )
  forM_ pairs $ \(coord, (c, pt)) -> do
    let hb = case c of
          White -> whiteHb
          Black -> blackHb
        pInd = fromEnum pt
    Bitboard v <- Hb.unsafeRead hb pInd
    Hb.unsafeWrite hb pInd $! Bitboard (v .|. toBit coord)
  w <- Hb.unsafeFreeze whiteHb
  b <- Hb.unsafeFreeze blackHb
  pure $ Board (w, b)

{-
  Unpacks a board to a 2D list following FEN order.
 -}
unpackToFenOrd :: Board -> [[Square]]
unpackToFenOrd bd = (fmap . fmap) (at bd) fenCoords

instance Show Board where
  show bd =
    intercalate "/"
      . fmap (concatMap tr . NE.groupBy ((==) `on` isNothing))
      $ unpackToFenOrd bd
    where
      tr :: NE.NonEmpty Square -> String
      tr xs@(v NE.:| _) = case v of
        Nothing -> show (length xs)
        Just _ -> toList $ fmap (pieceToChar . fromJust) xs

at :: Board -> Coord -> Square
at (Board (bs, ws)) c = asum $ zipWith go (toList bs <> toList ws) whats
  where
    cb = toBit c
    go :: Bitboard -> Piece -> Square
    go (Bitboard bb) v = v <$ guard (bb .&. cb /= 0)
    whats :: [Piece]
    whats =
      (,)
        <$> universe @Color
          <*> universe @PieceType

infoOccupied :: Board -> (Bitboard, Bitboard)
infoOccupied (Board (w, b)) = (foldr1 (.|.) (toList w), foldr1 (.|.) (toList b))

getHalfboard :: Board -> Color -> Halfboard
getHalfboard (Board (w, b)) c = case c of
  White -> w
  Black -> b

modifyHalfboard :: (Halfboard -> Halfboard) -> Color -> Board -> Board
modifyHalfboard f c (Board (w, b)) = case c of
  White -> Board (f w, b)
  Black -> Board (w, f b)

{-
  Note that this is consider low-level api and does not check
  the resulting Board at all - meaning it's possible to produce a Board
  in which different pieces are occupying the same square.
 -}
setBoardAt :: Piece -> Coord -> Bool -> Board -> Board
setBoardAt (color, pt) coord newVal =
  modifyHalfboard (Hb.modifyBitboard modify pt) color
  where
    modify :: Bitboard -> Bitboard
    modify (Bitboard bb) =
      Bitboard $
        if newVal
          then masked .|. cb
          else masked
      where
        cb = toBit coord
        masked = bb .&. complement cb

emptyHb :: Halfboard
emptyHb = Hb.empty

emptyBoard :: Board
emptyBoard = Board (emptyHb, emptyHb)

swapBoardSide :: Board -> Board
swapBoardSide (Board (w, b)) = Board (b, w)

hbAt :: Halfboard -> PieceType -> Bitboard
hbAt = Hb.at

{-
  Pretty-print a board similar to stockfish's `d` command.
 -}
pprBoard :: Board -> IO ()
pprBoard bd = do
  let vSep = intercalate "+" $ "" : replicate 8 "---" <> [""]
  putStrLn vSep
  forM_ (zip fenCoords [8 :: Int, 7 ..]) $ \(rankCoords, r) -> do
    let thisRank =
          fmap
            (\c -> maybe ' ' pieceToChar (at bd c))
            rankCoords
    putStrLn $
      "|" <> intercalate "|" (fmap (\c -> [' ', c, ' ']) thisRank) <> "| " <> show r
    putStrLn vSep
  putStrLn $ "  " <> intercalate "   " (fmap (: []) ['a' .. 'h'])
