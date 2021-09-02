{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeApplications #-}

module Game.Sxako.Move where

import Control.Monad
import Data.Bits
import Game.Sxako.Bitboard
import Game.Sxako.Board
import Game.Sxako.Coord
import Game.Sxako.Fen
import Game.Sxako.Types

{-
  Move from one coord to another.
  For a promotion move, a target piece type must be present.
 -}
data Ply
  = PlyNorm
      { pFrom :: Coord
      , pTo :: Coord
      }
  | PlyPromo
      { pFrom :: Coord
      , pTo :: Coord
      , pPiece :: PieceType
      }

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

todo :: a
todo = error "TODO"

{-
  for a ply-generating function:

  pg <record> <coord>

  we generate all legal plies from <coord>.

 -}
type PlyGen = Record -> Coord -> [Ply]

{-
  Pawn moves:

  - normal forward moves:
    + move forward one square:
      - if target square is not blocked
      - and is not a promotion (TODO: deal with promotion later)
    + move forward two squares: if two squares in front of it are not blocked.

  - capture moves: if target square is occupied by an opponent piece.
    - and is not a promotion (TODO: deal with promotion later)

  - TODO: en passant
 -}
pawnPlies :: PlyGen
pawnPlies Record {placement, activeColor} pFrom =
  forwardPlies <> todo
  where
    promoTargets = [Knight, Bishop, Rook, Queen]
    (rank, _) = withRankAndFile @Int pFrom (,)
    (wOccupied, bOccupied) = infoOccupied placement
    Bitboard bothOccupied = wOccupied .|. bOccupied
    (isHomeRank, isNextPromo) =
      case activeColor of
        White -> (rank == 1, rank == 6)
        Black -> (rank == 6, rank == 1)
    forwardPlies = do
      let dir = case activeColor of
            White -> DN
            Black -> DS
      guard $ not isNextPromo
      Just pNext <- pure (nextCoord dir pFrom)
      guard $ bothOccupied .&. toBit pNext == 0
      pure PlyNorm {pFrom, pTo = pNext} <> do
        guard isHomeRank
        Just pNext2 <- pure (nextCoord dir pNext)
        guard $ bothOccupied .&. toBit pNext2 == 0
        pure PlyNorm {pFrom, pTo = pNext2}
