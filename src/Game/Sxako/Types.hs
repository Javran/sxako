{-# LANGUAGE DataKinds #-}

module Game.Sxako.Types
  ( EightElems
  , PieceType (..)
  , Color (..)
  , Side (..)
  , universe
  )
where

import qualified Data.Vector.Fixed as VF

type EightElems = VF.VecList 8

{-
  pawns and kings might not be considered pieces,
  but let's not make it more complicated than needed.
 -}
data PieceType
  = Pawn
  | Knight
  | Bishop
  | Rook
  | Queen
  | King
  deriving (Enum, Eq, Ord, Bounded, Show)

{-
  TODO: Plan to implement all legal moves:

  If we ignore absolute pins and checks

  - Pawn:

    + can move forward one square if the target square is empty
    + can move forward two squares if on home square
    + normal capture rule
    + en passant
    + promotion

  - Knight:

    + normal knight move rule, target square just need to not be occupied
      by anything of own color.

  - Bishop & Rook:

    + anything non-empty stops it
    + can capture if the blocking square is opposite color.

  - Queen: just pretend it's both a Bishop and a Rook.

  - King:
    + normal moves
    + castling

  Now to consider absolute pins and checks,
  we just need to exclude moves that would result in King being checked.

 -}

data Color = White | Black deriving (Enum, Eq, Ord, Bounded, Show)

universe :: (Enum a, Bounded a) => [a]
universe = [minBound .. maxBound]

data Side = KingSide | QueenSide deriving (Eq, Ord, Show)
