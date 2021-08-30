{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}

module Game.Sxako.Board
  ( Board
  , Halfboard
  , fromPlacement
  , emptyHb
  , hbAt
  , at
  )
where

import Control.Monad
import Control.Monad.ST.Strict
import Data.Bits
import Data.Foldable
import Data.Maybe
import qualified Data.Vector.Fixed as VF
import qualified Data.Vector.Fixed.Boxed as VFB
import qualified Data.Vector.Fixed.Mutable as VFM
import Game.Sxako.Bitboard
import Game.Sxako.Coord
import Game.Sxako.Fen
import Game.Sxako.Types

{-
  There are multiple ways to represent a board with Bitboard as elements:

  - we can do a vector of 12 elements
  - or a pair of vectors of 6 elements in each

  And when it comes to the choice of boxed / unboxed vector, we can do both.

  For now performance doesn't matter, so let's just pick one and stick with it
  unless there are blocking problems along the way.

  I choose to use a pair of boxed vectors so to allow more sharing.
 -}
type Halfboard = VFB.Vec 6 Bitboard

emptyHb :: Halfboard
emptyHb = VF.replicate (Bitboard 0)

hbAt :: Halfboard -> PieceType -> Bitboard
hbAt hb pt = hb VF.! fromEnum pt

{-
  (<white side>, <black side>)
 -}
type Board = (Halfboard, Halfboard)

fromPlacement :: Placement -> Board
fromPlacement ps2d = runST $ do
  whiteHb <- VFM.thaw emptyHb
  blackHb <- VFM.thaw emptyHb
  let pairs :: [] (Coord, Piece)
      pairs =
        catMaybes
          (zipWith
             (\mcp c -> (c,) <$> mcp)
             (concat $ VF.toList $ fmap VF.toList ps2d)
             (concat fenCoords))
  forM_ pairs $ \(coord, (c, pt)) -> do
    let hb = case c of
          White -> whiteHb
          Black -> blackHb
        pInd = fromEnum pt
    Bitboard v <- VFM.unsafeRead hb pInd
    VFM.unsafeWrite hb pInd $! Bitboard (v .|. toBit coord)
  (,) <$> VFM.unsafeFreeze whiteHb <*> VFM.unsafeFreeze blackHb

at :: Board -> Coord -> Maybe Piece
at (bs, ws) c = asum $ zipWith go (toList bs <> toList ws) whats
  where
    cb = toBit c
    go :: Bitboard -> Piece -> Maybe Piece
    go (Bitboard bb) v = v <$ guard (bb .&. cb /= 0)
    whats :: [Piece]
    whats =
      (,)
        <$> universe @Color
          <*> universe @PieceType
