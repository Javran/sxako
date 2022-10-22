{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeApplications #-}

module Game.Sxako.Ply
  ( Ply (..)
  , GameResult (..)
  , DrawReason (..)
  , attackingSquares
  , legalPlies
  , getCheckType
  , isCastlePly
  , isCapturePly
  , legalPliesMap
  , legalPliesEither
  )
where

import Control.Applicative
import Control.Monad
import Control.Monad.Writer.CPS
import Data.Aeson
import Data.Bits
import Data.Hashable
import qualified Data.Map.Strict as M
import Data.Maybe
import qualified Data.Text as T
import Data.Word
import Game.Sxako.Bitboard
import Game.Sxako.Board
import Game.Sxako.Castling
import Game.Sxako.Common
import Game.Sxako.Coord as Coord
import Game.Sxako.Fen
import Text.ParserCombinators.ReadP

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
  deriving (Eq, Ord)

{-
  Note that the rule of Chess also declares that
  threefold (or fivefold) repetition be a draw,
  but `Record` type (which is meant to be equivalent to what FEN could encode)
  does not contain sufficient information for us to determine that.

  TODO: we might implement draw by repetition once we make it possible
  to have ply list as input.

 -}

data DrawReason
  = Stalemate
  | FiftyMoves
  | InsufficientMaterial
  deriving (Show)

data GameResult
  = ResultCheckmate Color -- color indicates the winner.
  | ResultDraw DrawReason
  deriving (Show)

{-
  Encode a Ply for hashing
 -}
encodePlyForHash :: Ply -> Word32
encodePlyForHash p = hFrom .|. shiftL hTo 8 .|. shiftL hTy 16
  where
    hFrom = fromIntegral $ Coord.toWord8 (pFrom p)
    hTo = fromIntegral $ Coord.toWord8 (pTo p)
    hTy = case p of
      PlyNorm {} -> maxBound
      PlyPromo {pPiece} -> fromIntegral (fromEnum pPiece)

instance Hashable Ply where
  hash = hash @Word32 . encodePlyForHash
  hashWithSalt s v = hashWithSalt @Word32 s (encodePlyForHash v)

instance Show Ply where
  show p =
    show (pFrom p) <> show (pTo p) <> case p of
      PlyNorm {} -> ""
      PlyPromo {pPiece} -> [pieceToChar (Black, pPiece)]

instance Read Ply where
  readsPrec _ = readP_to_S $ do
    let getCoord = readS_to_P @Coord reads
    pFrom <- getCoord
    pTo <- getCoord
    look >>= \case
      ch : _
        | ch `elem` ("nbrq" :: [] Char)
          , Just (_, pPiece) <- charToPiece ch ->
          PlyPromo {pFrom, pTo, pPiece} <$ get
      _ -> pure PlyNorm {pFrom, pTo}

instance FromJSON Ply where
  parseJSON = withText "Ply" $ \t -> do
    [(p, "")] <- pure $ reads (T.unpack t)
    pure p

instance FromJSONKey Ply

instance ToJSON Ply where
  toJSON p = String (T.pack $ show p)

instance ToJSONKey Ply

{-
  TODO: To improve performance,
  we can probably do with a vector of 6 elements for each piece type.
  This would allow us to avoid some list concats and make it easier
  when it comes to converting to SANs - in current implementation
  we basically just merge all plies together and later separate each piece types out,
  resulting in some unnecessary works.

  This will also allow us to generalize Halfboard into some PieceType-indexed vector.
 -}
legalPlies :: Record -> [(Ply, Record)]
legalPlies r@Record {placement = bd, activeColor} = do
  coord <- universe
  result@(_, Record {placement = bd'}) <- case at bd coord of
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
  guard $ hasSafeKings activeColor bd'
  pure result

legalPliesMap :: Record -> M.Map Ply Record
legalPliesMap = M.fromList . legalPlies

{-
  Note: it might be possible to merge some logic with `legalPliesEither` below,
  but I doubt it would worth the effort.
 -}
getCheckType :: Record -> Maybe CheckType
getCheckType r@Record {activeColor, placement} = do
  guard $ not (hasSafeKings activeColor placement)
  let hasLegalPlies = not . null . legalPlies $ r
  pure $ if hasLegalPlies then Check else Checkmate

isCastlePly :: Record -> Ply -> Maybe Side
isCastlePly Record {activeColor, placement} p = do
  {-
    Note that castling right is not checked here - this assumes that Record
    fields are self-consistent.
   -}
  let (kingInitCoord, (kSideCoord, qSideCoord)) = case activeColor of
        White -> (e1, (g1, c1))
        Black -> (e8, (g8, c8))
  guard $ at placement kingInitCoord == Just (activeColor, King)
  KingSide <$ guard (p == PlyNorm kingInitCoord kSideCoord)
    <|> QueenSide <$ guard (p == PlyNorm kingInitCoord qSideCoord)

isCapturePly :: Record -> Ply -> Bool
isCapturePly Record {placement, enPassantTarget} p =
  case targetSq of
    Just _ -> True
    Nothing -> case enPassantTarget of
      Just c | c == pTo p ->
        case at placement (pFrom p) of
          Just (_, Pawn) -> True
          _ -> False
      _ -> False
  where
    targetSq = at placement (pTo p)

{-
  TODO: to be tested.

  Either returns a non-empty list of possible next plies,
  or returns a Left to indicate that the game has concluded.

 -}
legalPliesEither :: Record -> Either GameResult [(Ply, Record)]
legalPliesEither
  record@Record
    { activeColor
    , placement
    , halfMove
    } = case legalPlies record of
    [] ->
      -- the active color has no legal move.
      Left $
        if hasSafeKings activeColor placement
          then ResultDraw Stalemate
          else ResultCheckmate (opposite activeColor)
    xs@(_ : _) ->
      if
          | halfMove > 100 -> Left $ ResultDraw FiftyMoves
          | insuffMat -> Left $ ResultDraw InsufficientMaterial
          | otherwise -> pure xs
      where
        [hbW, hbB] = fmap (getHalfboard placement) [White, Black]
        noHeavyPieceOrPawn :: Halfboard -> Bool
        noHeavyPieceOrPawn hb =
          all (\pt -> hbAt hb pt == Bitboard 0) [Pawn, Rook, Queen]
        countBishopAndKnight
          :: Halfboard
          -> ( Int {- knights -}
             , (Int {- dark bishops -}, Int {- light bishops -})
             )
        countBishopAndKnight hb = (knightCount, (darkB, lightB))
          where
            (Sum darkB, Sum lightB) =
              foldMap (\c -> if isDark c then (1, 0) else (0, 1)) $
                allSetCoords (hbAt hb Bishop)
            knightCount = popCount (hbAt hb Knight)
        {-
          The following are cases of insufficient materials:

          - king versus king
          - king and bishop versus king
          - king and knight versus king
          - king and bishop versus king and bishop with the bishops on the same color.

          Observation:
          (1) does not qualify if any of queen, rook, pawn remains
          (2) otherwise count (knight, (light bishop, dark bishop)) for both sides,
            which should give us sufficient info to perform this IM check.
         -}
        insuffMat =
          -- Observation (1)
          noHeavyPieceOrPawn hbW
            && noHeavyPieceOrPawn hbB
            &&
            -- Observation (2)
            let (wKnights, (wDBishops, wLBishops)) = countBishopAndKnight hbW
                (bKnights, (bDBishops, bLBishops)) = countBishopAndKnight hbB
             in (wKnights + wDBishops + wLBishops + bKnights + bDBishops + bLBishops <= 1)
                  || (-- no knights
                      (wKnights, bKnights) == (0, 0)
                        &&
                        -- exactly one bishop on both sides
                        wDBishops + wLBishops == 1
                        && bDBishops + bLBishops == 1
                        &&
                        -- same color bishops
                        ((wDBishops, bDBishops) == (1, 1)
                           || (wLBishops, bLBishops) == (1, 1)))

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
  coord <- allSetCoords pieceBd
  let cs :: [Coord]
      cs = attackingSquaresAux bd (c, pt) coord
  Bitboard . toBit <$> cs

{-
  Internal type synonym.

  A value of this type:

  - generates possible moves assuming the corresponding piece is on the input coord.
  - always calls `finalize` in the end.

  Note that it is intentional that we don't verify whether a ply would put one's king in check.
  This is because if we do so, we are making the assumption that
  advancing n+1 steps is possible when advancing n steps is possible.
  This is not true however: if a long ranged piece have to
  go to some specific squares to cover a check (or a pawn
  has to double advance to cover a check), those intermediate
  squares won't be available.

  To implement this correctly, we have two passes of filtering:
  one to verify that the move is possible ignoring king safety (with list monad)
  and another to rule out those that puts ones king in check.

  TODO: probably we can do this in `StateT Record []` so there's less
  explicit passing around.

 -}
type PlyGen = Record -> Coord -> [(Ply, Record)]

data EnPassantReset = EPKeep | EPClear

data HalfMoveReset = HMIncr | HMReset

{-
  It's literally impossible in standard Chess
  to get multiple kings of the same color.
  but the data representation allows it,
  so we choose to deal with this situation anyway.
  Here let's just say we are fine as long as:

  - we don't have kings at all (might happen in tests)
  - not all kings are in check.

 -}
hasSafeKings :: Color -> Board -> Bool
hasSafeKings c bd = kings == Bitboard 0 || ((kings .&. oppoAttacking) /= kings)
  where
    oppoColor = opposite c
    kings = hbAt (getHalfboard bd c) King
    oppoAttacking = attackingSquares bd oppoColor

{-
  All PlyGen must be finalized with this function, it updates:
  - activeColor
  - castling
  - enPassantTarget
  - halfMove
  - fullMove

  TODO: would `Endo Record` be more concise than providing option as ADT values?
 -}
finalize :: EnPassantReset -> HalfMoveReset -> Ply -> Record -> (Ply, Record)
finalize
  epReset
  hmReset
  ply
  r@Record
    { activeColor
    , castling
    , enPassantTarget
    , halfMove
    , fullMove
    } =
    let oppoColor = opposite activeColor
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
            else
              let Endo update = execWriter $ do
                    {-
                      accumulate modifiers so we can combine results from testing both plyTo and plyFrom
                      rather than ignoring one of them.
                     -}
                    let cond ~> fn =
                          when cond $ tell (Endo fn)
                    case activeColor of
                      White -> do
                        (plyTo == a8) ~> (`minusCastleRight` blackQueenSide)
                        (plyTo == h8) ~> (`minusCastleRight` blackKingSide)
                        (plyFrom == e1) ~> removeCastleRight White
                        (plyFrom == a1) ~> (`minusCastleRight` whiteQueenSide)
                        (plyFrom == h1) ~> (`minusCastleRight` whiteKingSide)
                      Black -> do
                        (plyTo == a1) ~> (`minusCastleRight` whiteQueenSide)
                        (plyTo == h1) ~> (`minusCastleRight` whiteKingSide)
                        (plyFrom == e8) ~> removeCastleRight Black
                        (plyFrom == a8) ~> (`minusCastleRight` blackQueenSide)
                        (plyFrom == h8) ~> (`minusCastleRight` blackKingSide)
               in update castling
     in ( ply
        , r
            { activeColor = oppoColor
            , castling = castling'
            , enPassantTarget =
                case epReset of
                  EPKeep -> enPassantTarget
                  EPClear -> Nothing
            , halfMove =
                case hmReset of
                  HMIncr -> halfMove + 1
                  HMReset -> 0
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
      moveToMightPromo pNext curBd =
        if isNextPromo
          then do
            pPiece <- promoTargets
            pure $
              finalize
                EPClear
                HMReset
                PlyPromo {pFrom, pTo = pNext, pPiece}
                record
                  { placement =
                      setBoardAt (activeColor, pPiece) pNext True curBd
                  }
          else
            pure $
              finalize
                EPClear
                HMReset
                PlyNorm {pFrom, pTo = pNext}
                record
                  { placement =
                      setBoardAt (activeColor, Pawn) pNext True curBd
                  }
      advances = do
        Just pNext <- pure (nextCoord advanceDir pFrom)
        guard $ not (testBoard bothOccupied pNext)
        let pNextPlies = moveToMightPromo pNext bd1
        pNextPlies <> do
          -- double advance.
          guard isHomeRank
          Just pNext2 <- pure (nextCoord advanceDir pNext)
          guard $ not (testBoard bothOccupied pNext2)
          let couldCaptureSq = do
                Just cSq <-
                  withRankAndFile @Int
                    pNext2
                    (\rInd fInd ->
                       fromRankAndFile rInd <$> [fInd -1, fInd + 1])
                case at bd1 cSq of
                  Just (c, Pawn) | c == opposite activeColor -> pure cSq
                  _ -> []
          pure $
            finalize
              EPKeep
              HMReset
              PlyNorm {pFrom, pTo = pNext2}
              record
                { placement =
                    setBoardAt (activeColor, Pawn) pNext2 True bd1
                , enPassantTarget =
                    -- google en passant
                    if null couldCaptureSq
                      then Nothing
                      else Just pNext
                }
      captures = do
        captureDir <- captureDirs
        Just pNext <- pure (nextCoord captureDir pFrom)
        let isEnPassant = enPassantTarget == Just pNext
            targetSq = at bd1 pNext
            oppoColor = opposite activeColor
        guard $ testBoard opponentOccupied pNext || isEnPassant
        let bd2 =
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
                   in setBoardAt (oppoColor, Pawn) epCaptureSq False bd1
                else
                  let Just p = targetSq
                   in setBoardAt p pNext False bd1

        moveToMightPromo pNext bd2

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
        (bd3, hmReset) = case at bd0 pTo of
          Nothing -> (bd2, HMIncr)
          Just p -> (setBoardAt p pTo False bd2, HMReset)
    pure $
      finalize
        EPClear
        hmReset
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
        fin = finalize EPClear
    case at bd1 curCoord of
      Just (targetColor, targetPt) ->
        if targetColor == activeColor
          then []
          else do
            let bd2 = setBoardAt (opposite activeColor, targetPt) curCoord False bd1
                bd3 = setBoardAt (activeColor, pt) curCoord True bd2
            pure $ fin HMReset PlyNorm {pFrom, pTo = curCoord} record {placement = bd3}
      Nothing -> do
        let bd2 = setBoardAt (activeColor, pt) curCoord True bd1
            p = fin HMIncr PlyNorm {pFrom, pTo = curCoord} record {placement = bd2}
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
      fin = finalize EPClear
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
                 in pure $
                      fin
                        HMReset
                        PlyNorm {pFrom, pTo}
                        record
                          { placement = bd3
                          }
          Nothing ->
            -- a simple move.
            pure $
              fin
                HMIncr
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
        pure $ fin HMIncr (PlyNorm {pFrom, pTo = kingTo}) record {placement = bd2, castling}
