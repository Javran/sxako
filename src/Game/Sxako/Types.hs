{-# LANGUAGE DataKinds #-}

module Game.Sxako.Types
  ( EightElems
  , PieceType (..)
  , Color (..)
  , Piece
  , Side (..)
  , Placement2D
  , Square
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

data Color = White | Black deriving (Enum, Eq, Ord, Bounded, Show)

type Piece = (Color, PieceType)

universe :: (Enum a, Bounded a) => [a]
universe = [minBound .. maxBound]

data Side = KingSide | QueenSide deriving (Eq, Ord, Show)

{-
  2D list of a Chess board - used for intermediate representations.
 -}
type Placement2D = EightElems (EightElems (Maybe Piece))

{-
  Information of one sqaure: empty or there's something on it.
 -}
type Square = Maybe Piece
