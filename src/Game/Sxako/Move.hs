{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeApplications #-}

module Game.Sxako.Move
  ( Ply (..)
  , attackingSquares
  , legalPlies
  )
where

import Control.Monad
import Data.Bits
import qualified Data.Map.Strict as M
import Data.Maybe
import Game.Sxako.Bitboard
import Game.Sxako.Board
import Game.Sxako.Castling
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
  deriving (Show, Eq, Ord)

legalPlies :: Record -> M.Map Ply Record
legalPlies r@Record {placement = bd, activeColor} = M.fromList $ do
  coord <- universe
  case at bd coord of
    Just (color, pt)
      | color == activeColor ->
        case pt of
          Pawn -> pawnPlies r coord
          Knight -> knightPlies r coord
          Bishop -> bishopPlies r coord
          Rook -> rookPlies r coord
          Queen -> queenPlies r coord
          King -> kingPlies r coord
    _ -> []

{-
  Auxilary function to figure out squares being attacked.

  - It doesn't matter that much whether an occupied square should be
    considered being attacked, the design choice here is to consider they are
    to reduce the amount of testing.
  - En passant rule is not taken into account in this function.
 -}
attackingSquaresAux :: Board -> Piece -> Coord -> [Coord]
attackingSquaresAux bd (color, pt) coord = case pt of
  Pawn -> do
    dir <- case color of
      White -> [DNW, DNE]
      Black -> [DSW, DSE]
    maybeToList (nextCoord dir coord)
  Knight -> do
    let (rank, file) = withRankAndFile @Int coord (,)
    (lR, lF) <- [(1, 2), (2, 1)]
    sR <- [-1, 1]
    sF <- [-1, 1]
    let nextRank = rank + sR * lR
        nextFile = file + sF * lF
    maybeToList (fromRankAndFile nextRank nextFile)
  Bishop -> torpedoes diagonalDirs
  Rook -> torpedoes straightDirs
  Queen -> torpedoes allDirs
  King -> do
    dir <- allDirs
    maybeToList (nextCoord dir coord)
  where
    (wOccupied, bOccupied) = infoOccupied bd
    bothOccupied = wOccupied .|. bOccupied

    {-
      keep moving and collecting squares in one direction
      as long as current square is empty.
      When hitting something non-empty, that square is collected.
     -}
    torpedo :: Dir -> Coord -> [Coord]
    torpedo d curCoord = case testBoard bothOccupied curCoord of
      False ->
        -- TODO: a bit awkward here.
        case nextCoord d curCoord of
          Nothing -> pure curCoord
          Just c' -> do
            curCoord : torpedo d c'
      True -> pure curCoord
    {- see `torpedo` -}
    torpedoes :: [Dir] -> [Coord]
    torpedoes ds = do
      dir <- ds
      coord' <- maybeToList (nextCoord dir coord)
      torpedo dir coord'

attackingSquares :: Board -> Color -> Bitboard
attackingSquares bd c = foldr (.|.) (Bitboard 0) $ do
  pt <- [Pawn, Knight, Bishop, Rook, Queen, King]
  let hb = getHalfboard bd c
      pieceBd = hbAt hb pt
  coord <- allCoords
  guard $ testBoard pieceBd coord
  let cs :: [Coord]
      cs = attackingSquaresAux bd (c, pt) coord
  pure $ Bitboard $ foldr (.|.) 0 (fmap toBit cs)

{-
  Internal type synonym.

  A value of this type:

  - generates possible moves assuming the corresponding piece is on the input coord.
  - always calls `finalize` in the end.

    `finalize` function updates most of the fields of the Record and
    remove moves that put their own king in check
    (addressing absolute pin).

  The expectation is that as long as input Record is valid,
  PlyGen always generate only valid plies and Records.

  TODO: probably we can do thins in `StateT Record []` so there's less
  explicit passing around.

 -}
type PlyGen = Record -> Coord -> [(Ply, Record)]

{-
  All PlyGen must be finalized with this function.

  - it checks whether king of the active color is in check
    and reject those moves that put their king in check
    (dealing with absolute pins).
  - it updates `activeColor`, `castling`, `enPassantTarget`, `halfMove` and `fullMove`.

 -}
finalize :: Bool -> Bool -> Ply -> Record -> [(Ply, Record)]
finalize
  clearEnPassant
  resetHalfMove
  ply
  r@Record
    { placement = bd
    , activeColor
    , castling
    , enPassantTarget
    , halfMove
    , fullMove
    } = do
    let oppoColor = opposite activeColor
        kings = hbAt (getHalfboard bd activeColor) King
        oppoAttacking = attackingSquares bd oppoColor
        plyTo = pTo ply
        plyFrom = pFrom ply
        castling' =
          {-
            Note that We don't actually check whether pieces are actually
            on the right places - this relys on input Record being consistent
            with itself.
           -}
          if castling == none
            then castling
            else case activeColor of
              White ->
                if
                    | plyTo == a8 -> castling `minusCastleRight` blackQueenSide
                    | plyTo == h8 -> castling `minusCastleRight` blackKingSide
                    | plyFrom == e1 -> removeCastleRight White castling
                    | plyFrom == a1 -> castling `minusCastleRight` whiteQueenSide
                    | plyFrom == h1 -> castling `minusCastleRight` whiteKingSide
                    | otherwise -> castling
              Black ->
                if
                    | plyTo == a1 -> castling `minusCastleRight` whiteQueenSide
                    | plyTo == h1 -> castling `minusCastleRight` whiteKingSide
                    | plyFrom == e8 -> removeCastleRight Black castling
                    | plyFrom == a8 -> castling `minusCastleRight` blackQueenSide
                    | plyFrom == h8 -> castling `minusCastleRight` blackKingSide
                    | otherwise -> castling
    {-
      It's literally impossible in standard Chess
      to get multiple kings of the same color.
      but the data representation allows it,
      so we choose to deal with this situation anyway.
      Here let's just say we are fine as long as:

      - we don't have kings at all (might happen in tests)
      - not all kings are in check.

     -}
    guard $ kings == Bitboard 0 || ((kings .&. oppoAttacking) /= kings)
    pure
      ( ply
      , r
          { activeColor = oppoColor
          , castling = castling'
          , enPassantTarget = if clearEnPassant then Nothing else enPassantTarget
          , halfMove = if resetHalfMove then 0 else halfMove + 1
          , fullMove = if activeColor == Black then fullMove + 1 else fullMove
          }
      )

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
  record@Record
    { placement
    , activeColor
    , enPassantTarget
    }
  pFrom =
    advances <> captures
    where
      -- always reset halfMove as this is a pawn move.
      fin = finalize False True
      -- remove pawn at current coord.
      bd1 =
        setBoardAt (activeColor, Pawn) pFrom False placement
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
      -- move pawn to target also handle potential promotion.
      moveToMightPromo pNext =
        if isNextPromo
          then do
            pPiece <- promoTargets
            fin
              PlyPromo {pFrom, pTo = pNext, pPiece}
              record
                { placement =
                    setBoardAt (activeColor, pPiece) pNext True bd1
                }
          else
            fin
              PlyNorm {pFrom, pTo = pNext}
              record
                { placement =
                    setBoardAt (activeColor, Pawn) pNext True bd1
                }
      advances = do
        Just pNext <- pure (nextCoord advanceDir pFrom)
        guard $ not (testBoard bothOccupied pNext)
        let pNextPlies = moveToMightPromo pNext
        pNextPlies <> do
          -- double advance.
          guard isHomeRank
          Just pNext2 <- pure (nextCoord advanceDir pNext)
          guard $ not (testBoard bothOccupied pNext2)
          fin
            PlyNorm {pFrom, pTo = pNext2}
            record
              { placement =
                  setBoardAt (activeColor, Pawn) pNext2 True bd1
              , enPassantTarget =
                  -- google en passant
                  Just pNext
              }
      captures = do
        captureDir <- captureDirs
        Just pNext <- pure (nextCoord captureDir pFrom)
        let isEnPassant = enPassantTarget == Just pNext
            targetSq = at bd1 pNext
            oppoColor = opposite activeColor
        guard $ testBoard opponentOccupied pNext || isEnPassant
        -- handle promotion first then remove captured opponent piece.
        (ply, record'@Record {placement = bd2}) <- moveToMightPromo pNext
        fin
          ply
          record'
            { placement =
                if isEnPassant
                  then -- holy hell

                    let Just epCaptureSq =
                          withRankAndFile @Int
                            pNext
                            (\rInd fInd ->
                               fromRankAndFile
                                 (case oppoColor of
                                    White -> rInd + 1
                                    Black -> rInd -1)
                                 fInd)
                     in setBoardAt (oppoColor, Pawn) epCaptureSq False bd2
                  else
                    let Just p = targetSq
                     in setBoardAt p pNext False bd2
            }

knightPlies :: PlyGen
knightPlies
  record@Record
    { placement = bd0
    , activeColor
    }
  pFrom = do
    let bd1 = setBoardAt (activeColor, Knight) pFrom False bd0
        (rank, file) = withRankAndFile @Int pFrom (,)
        (wOccupied, bOccupied) = infoOccupied bd0
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
    let bd2 = setBoardAt (activeColor, Knight) pTo True bd1
        bd3 = case at bd0 pTo of
          Nothing -> bd2
          Just p -> setBoardAt p pTo False bd2
    finalize
      True
      False
      PlyNorm {pFrom, pTo}
      record {placement = bd3}

{-
  Goes in one direction when the square is empty.
  Stops at the blocking square.
  Can capture if the blockng square is an empty piece.
 -}
oneDirPlies :: PieceType -> Dir -> Coord -> PlyGen
oneDirPlies
  pt
  dir
  curCoord
  record@Record {placement = bd0, activeColor}
  pFrom = do
    let bd1 = setBoardAt (activeColor, pt) pFrom False bd0
        fin = finalize True False
    case at bd1 curCoord of
      Just (targetColor, targetPt) ->
        if targetColor == activeColor
          then []
          else do
            let bd2 = setBoardAt (opposite activeColor, targetPt) curCoord False bd1
                bd3 = setBoardAt (activeColor, pt) curCoord True bd2
            fin PlyNorm {pFrom, pTo = curCoord} record {placement = bd3}
      Nothing -> do
        let bd2 = setBoardAt (activeColor, pt) curCoord True bd1
        p <- fin PlyNorm {pFrom, pTo = curCoord} record {placement = bd2}
        [p]
          <> case nextCoord dir curCoord of
            Just c' -> oneDirPlies pt dir c' record pFrom
            Nothing -> []

bishopPlies :: PlyGen
bishopPlies r pFrom = do
  d <- diagonalDirs
  c' <- maybeToList (nextCoord d pFrom)
  oneDirPlies Bishop d c' r pFrom

rookPlies :: PlyGen
rookPlies r pFrom = do
  d <- straightDirs
  c' <- maybeToList (nextCoord d pFrom)
  oneDirPlies Rook d c' r pFrom

queenPlies :: PlyGen
queenPlies r pFrom = do
  d <- allDirs
  c' <- maybeToList (nextCoord d pFrom)
  oneDirPlies Queen d c' r pFrom

{-
  Returns a Bitboard with only those coords set.
 -}
coordsToBitboard :: [Coord] -> Bitboard
coordsToBitboard = foldr (\c a -> Bitboard (toBit c) .|. a) (Bitboard 0)

data CastleRule = CastleRule
  { kingFromTo :: (Coord, Coord)
  , rookFromTo :: (Coord, Coord)
  , requireEmpty :: Bitboard
  , requireNoAttack :: Bitboard
  , castleRight :: Castling
  }

castleRule :: Color -> Side -> CastleRule
castleRule c s = case (c, s) of
  (White, KingSide) ->
    CastleRule
      { kingFromTo = (e1, g1)
      , rookFromTo = (h1, f1)
      , requireEmpty = coordsToBitboard [f1, g1]
      , requireNoAttack = coordsToBitboard [e1, f1, g1]
      , castleRight
      }
  (White, QueenSide) ->
    CastleRule
      { kingFromTo = (e1, c1)
      , rookFromTo = (a1, d1)
      , requireEmpty = coordsToBitboard [b1, c1, d1]
      , requireNoAttack = coordsToBitboard [c1, d1, e1]
      , castleRight
      }
  (Black, KingSide) ->
    CastleRule
      { kingFromTo = (e8, g8)
      , rookFromTo = (h8, f8)
      , requireEmpty = coordsToBitboard [f8, g8]
      , requireNoAttack = coordsToBitboard [e8, f8, g8]
      , castleRight
      }
  (Black, QueenSide) ->
    CastleRule
      { kingFromTo = (e8, c8)
      , rookFromTo = (a8, d8)
      , requireEmpty = coordsToBitboard [b8, c8, d8]
      , requireNoAttack = coordsToBitboard [c8, d8, e8]
      , castleRight
      }
  where
    castleRight = getCastleRight c s

kingPlies :: PlyGen
kingPlies
  record@Record
    { placement = bd0
    , activeColor
    , castling
    }
  pFrom = normalKingPlies <> castlePlies
    where
      fin = finalize True False
      bd1 = setBoardAt (activeColor, King) pFrom False bd0
      normalKingPlies = do
        {-
          One square any direction
         -}
        d <- allDirs
        pTo <- maybeToList (nextCoord d pFrom)
        let bd2 = setBoardAt (activeColor, King) pTo True bd1
        case at bd1 pTo of
          Just (targetColor, targetPt) ->
            if targetColor == activeColor
              then []
              else -- capture.

                let bd3 = setBoardAt (opposite activeColor, targetPt) pTo False bd2
                 in fin
                      PlyNorm {pFrom, pTo}
                      record
                        { placement = bd3
                        }
          Nothing ->
            -- a simple move.
            fin
              PlyNorm {pFrom, pTo}
              record
                { placement = bd2
                }
      castlePlies = do
        side <- [KingSide, QueenSide]
        let (wOccupied, bOccupied) = infoOccupied bd1
            bothOccupied = wOccupied .|. bOccupied

            CastleRule
              { kingFromTo = (_kingFrom, kingTo)
              , rookFromTo = (rookFrom, rookTo)
              , requireEmpty
              , requireNoAttack
              , castleRight
              } = castleRule activeColor side
        {-
          Here we assume `castling` is properly updated, so we don't
          have to actually check whether rook is in its original place.
         -}
        guard $
          (castleRight .&. castling /= none)
            && (requireEmpty .&. bothOccupied == Bitboard 0)
            && (requireNoAttack .&. attackingSquares bd1 (opposite activeColor) == Bitboard 0)
        let bd2 =
              setBoardAt (activeColor, Rook) rookFrom False
                . setBoardAt (activeColor, Rook) rookTo True
                . setBoardAt (activeColor, King) kingTo True
                $ bd1
        fin (PlyNorm {pFrom, pTo = kingTo}) record {placement = bd2, castling}
