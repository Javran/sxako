{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}

module Game.Sxako.Board
  ( Board
  , Halfboard
  , fromPlacement2d
  , emptyHb
  , hbAt
  , at
  , infoOccupied
  , pprBoard
  )
where

import Control.Monad
import Control.Monad.ST.Strict
import Data.Bits
import Data.Foldable
import Data.Function
import Data.List
import qualified Data.List.NonEmpty as NE
import Data.Maybe
import qualified Data.Vector.Fixed as VF
import qualified Data.Vector.Fixed.Boxed as VFB
import qualified Data.Vector.Fixed.Mutable as VFM
import Game.Sxako.Bitboard
import Game.Sxako.Coord
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
newtype Board = Board (Halfboard, Halfboard)

fromPlacement2d :: Placement2D -> Board
fromPlacement2d ps2d = runST $ do
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
  w <- VFM.unsafeFreeze whiteHb
  b <- VFM.unsafeFreeze blackHb
  pure $ Board (w, b)

unpackFenOrd :: Board -> [[Square]]
unpackFenOrd bd = (fmap . fmap) (at bd) fenCoords

instance Show Board where
  show bd =
    intercalate "/"
      . fmap (concatMap tr . NE.groupBy ((==) `on` isNothing))
      $ unpackFenOrd bd
    where
      tr :: NE.NonEmpty Square -> String
      tr xs@(v NE.:| _) = case v of
        Nothing -> show (length xs)
        Just _ -> toList $ fmap (pprPiece . fromJust) xs

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
infoOccupied (Board (w, b)) = (foldr1 (.|.) w, foldr1 (.|.) b)

pprPiece :: Piece -> Char
pprPiece (c, pt) = cs !! pInd
  where
    pInd = fromEnum pt
    cs = case c of
      White -> "PNBRQK"
      Black -> "pnbrqk"

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
            (\c -> maybe ' ' pprPiece (at bd c))
            rankCoords
    putStrLn $
      "|" <> intercalate "|" (fmap (\c -> [' ', c, ' ']) thisRank) <> "| " <> show r
    putStrLn vSep
  putStrLn $ "  " <> intercalate "   " (fmap (: []) ['a' .. 'h'])
