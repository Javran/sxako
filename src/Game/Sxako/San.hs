module Game.Sxako.San () where

{-
  Short Algebraic Notation
 -}

import Game.Sxako.Coord
import Game.Sxako.Move
import Game.Sxako.Types

data San
  = SNorm
      { sPieceFrom :: PieceType
      , sFrom :: Maybe Disamb
      , sTo :: Coord
      , sCapture :: Bool
      , sCheck :: Maybe CheckType
      , sPromo :: Maybe PieceType
      }
  | SCastle
      { sSide :: Side
      , sCheck :: Maybe CheckType
      }

data Disamb
  = DisambByFile Int
  | DisambByRank Int
  | DisambByCoord Coord

data CheckType = Check | Checkmate
