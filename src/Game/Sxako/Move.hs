{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeApplications #-}

module Game.Sxako.Move where

import Control.Monad
import Data.Bits
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
  deriving (Show)

{-
  TODO: Plan to implement all legal moves:

  If we ignore absolute pins and checks

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

  TODO: for now whether king is in check is not tested.
  TODO: PlyGen should also return updated Record.

 -}
type PlyGen = Record -> Coord -> [Ply]

{-
  Pawn moves:

  - normal advancing moves:
    + advance one square:
      - if target square is not blocked
    + advance two squares:
      - if we can advance one square
      - if target square is not blocked

  - capture moves:
    + if target square is occupied by an opponent piece.
    + en passant
 -}
pawnPlies :: PlyGen
pawnPlies
  Record
    { placement
    , activeColor
    , enPassantTarget
    }
  pFrom =
    advances <> captures
    where
      promoTargets = [Knight, Bishop, Rook, Queen]
      (rank, _) = withRankAndFile @Int pFrom (,)
      (wOccupied, bOccupied) = infoOccupied placement
      bothOccupied = wOccupied .|. bOccupied
      ( isHomeRank
        , isNextPromo
        , advanceDir
        , captureDirs
        , opponentOccupied
        ) =
          case activeColor of
            White ->
              ( rank == 1
              , rank == 6
              , DN
              , [DNW, DNE]
              , bOccupied
              )
            Black ->
              ( rank == 6
              , rank == 1
              , DS
              , [DSW, DSE]
              , wOccupied
              )
      advances = do
        Just pNext <- pure (nextCoord advanceDir pFrom)
        guard $ not (testBoard bothOccupied pNext)
        let pNextPlies =
              if isNextPromo
                then do
                  pPiece <- promoTargets
                  pure PlyPromo {pFrom, pTo = pNext, pPiece}
                else pure PlyNorm {pFrom, pTo = pNext}
        pNextPlies <> do
          guard isHomeRank
          Just pNext2 <- pure (nextCoord advanceDir pNext)
          guard $ not (testBoard bothOccupied pNext2)
          pure PlyNorm {pFrom, pTo = pNext2}
      captures = do
        captureDir <- captureDirs
        Just pNext <- pure (nextCoord captureDir pFrom)
        guard $
          testBoard opponentOccupied pNext
            || enPassantTarget == Just pNext
        if isNextPromo
          then do
            pPiece <- promoTargets
            pure PlyPromo {pFrom, pTo = pNext, pPiece}
          else pure PlyNorm {pFrom, pTo = pNext}

knightPlies :: PlyGen
knightPlies
  Record
    { placement
    , activeColor
    }
  pFrom = do
    let (rank, file) = withRankAndFile @Int pFrom (,)
        (wOccupied, bOccupied) = infoOccupied placement
        occupied = case activeColor of
          White -> wOccupied
          Black -> bOccupied
    (lR, lF) <- [(1, 2), (2, 1)]
    sR <- [-1, 1]
    sF <- [-1, 1]
    let nextRank = rank + sR * lR
        nextFile = file + sF * lF
    Just pTo <- pure (fromRankAndFile nextRank nextFile)
    guard $ not (testBoard occupied pTo)
    pure PlyNorm {pFrom, pTo}
