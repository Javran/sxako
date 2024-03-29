{-# LANGUAGE ViewPatterns #-}

module Game.Sxako.San (
  San (..),
  Disamb (..),
  CheckType (..),
  sanP,
  legalSansEither,
  legalPliesWithMapping,
) where

{-
  Short Algebraic Notation
 -}

import Control.Applicative
import Control.Monad
import Data.Attoparsec.ByteString.Char8 as Parser
import Data.Bifunctor
import Data.Char
import Data.Either
import Data.Foldable
import qualified Data.Map.Merge.Strict as M
import qualified Data.Map.Strict as M
import Data.Tuple
import Game.Sxako.Board
import Game.Sxako.Common
import Game.Sxako.Coord
import Game.Sxako.Fen
import Game.Sxako.Ply

data San
  = SNorm
      { sPieceFrom :: PieceType
      , sFrom :: Maybe Disamb
      , sCapture :: Bool
      , sTo :: Coord
      , sPromo :: Maybe PieceType
      , sCheck :: Maybe CheckType
      }
  | SCastle
      { sSide :: Side
      , sCheck :: Maybe CheckType
      }
  deriving (Eq, Ord, Generic)

instance NFData San

data Disamb
  = DisambByFile Int
  | DisambByRank Int
  | DisambByCoord Coord
  deriving (Show, Eq, Ord, Generic)

instance NFData Disamb

instance Read San where
  readsPrec _ = readsByAttoparsecChar8 sanP

instance Show San where
  show x =
    ( case x of
        SNorm {sPieceFrom, sFrom, sCapture, sTo, sPromo} ->
          concat
            [ if sPieceFrom == Pawn
                then ""
                else [pieceToChar (White, sPieceFrom)]
            , case sFrom of
                Nothing -> ""
                Just (DisambByFile fInd) -> [['a' .. 'h'] !! fInd]
                Just (DisambByRank rInd) -> [['1' .. '8'] !! rInd]
                Just (DisambByCoord c) -> show c
            , if sCapture then "x" else ""
            , show sTo
            , case sPromo of
                Nothing -> ""
                Just p -> '=' : [pieceToChar (White, p)]
            ]
        SCastle {sSide} ->
          case sSide of
            KingSide -> "O-O"
            QueenSide -> "O-O-O"
    )
      <> showCheck
    where
      showCheck = case sCheck x of
        Nothing -> ""
        Just Check -> "+"
        Just Checkmate -> "#"

rankP :: Parser Int
rankP = do
  ch <- satisfy (\ch -> ch >= '1' && ch <= '8')
  pure $ ord ch - ord '1'

fileP :: Parser Int
fileP = do
  ch <- satisfy (\ch -> ch >= 'a' && ch <= 'h')
  pure $ ord ch - ord 'a'

coordP :: Parser Coord
coordP = flip unsafeFromRankAndFile <$> fileP <*> rankP

pieceTypeP :: Parser PieceType
pieceTypeP =
  peekChar >>= \case
    Just ch
      | Just (White, pt) <- charToPiece ch ->
        pt <$ anyChar
    _ -> fail "unknown piece type"

disambP :: Parser (Maybe Disamb)
disambP = option Nothing (postCheck startWithFileP <|> startWithRankP)
  where
    startWithFileP = do
      fInd <- fileP
      option (DisambByFile fInd) $ do
        rInd <- rankP
        pure $ DisambByCoord $ unsafeFromRankAndFile rInd fInd
    startWithRankP = Just . DisambByRank <$> rankP

    {-
      Run the parser and make sure we are not mistakenly consuming Coords
      that are not supposed to be consumed as disambiguating term.
     -}
    postCheck p = do
      r <- p
      peekChar >>= \case
        Just ch | isLower ch -> pure (Just r)
        _ -> fail "disambP consumed too much"

captureP :: Parser Bool
captureP = option False (True <$ char 'x')

promoP :: Parser (Maybe PieceType)
promoP =
  option Nothing $
    Just <$> do
      _ <- char '='
      pt <- pieceTypeP
      guard $ pt /= Pawn && pt /= King
      pure pt

checkP :: Parser (Maybe CheckType)
checkP =
  option Nothing $
    Just
      <$> ( (Check <$ char '+')
              <|> (Checkmate <$ char '#')
          )

sanP :: Parser San
sanP = castleP <|> normalMoveP
  where
    castleP = do
      _ <- string "O-O"
      mCh <- peekChar
      sSide <- case mCh of
        Just '-' -> QueenSide <$ string "-O"
        _ -> pure KingSide
      sCheck <- checkP
      pure SCastle {sSide, sCheck}

    pieceFromP :: Parser PieceType
    pieceFromP =
      option Pawn pieceTypeP

    normalMoveP :: Parser San
    normalMoveP = do
      sPieceFrom <- pieceFromP
      sFrom <- disambP
      sCapture <- captureP
      sTo <- coordP
      sPromo <- promoP
      sCheck <- checkP
      pure $
        SNorm
          { sPieceFrom
          , sFrom
          , sCapture
          , sTo
          , sPromo
          , sCheck
          }

{-
  TODO: Note that currently we only have a "picky" API for applying a San,
  meaning checks, captures must be marked exactly to find the next board.

  But in future we need some sort of way to allow user to only specify
  piece type and target location.
  In this proposed API, resulting value would be a list to address ambiguity,
  so that `San` is closer to a query than a precise move.

  In this form, `from`, `capture` and `check` are all optional.
  But if they are specified, it must be accounted for in the result.

  e.g. `Bh7` could mean `Bxh7`, `Bxh7+`, `Bxh7#`, `Bbh7`, `Bbxh7+` etc.
  but if one gives `Bxh7+`, those without captures and checks are excluded from the results.
  (Checkmate and check without mate are considiered different and incompatible notations,
  this decision is somewhat arbitrary, we might revisit later.)

  Note: King is allowed to move 2 squares to indicate castling should the current board state allow.

  Note: to potentially support Chess960, we might internally switch to use
  king-captures-his-own-rook move to indicate castling.

  Note: resulting list must pair resulting board with precise San.

 -}

{-
  TODO: currently doing this the stupid way.

  By "stupid", the current way performs movegen twice, and find Ply-San pairs
  by matching resulting game record. Ideally this should be done during movegen.

 -}
legalPliesWithMapping :: Record -> (M.Map Ply Record, (M.Map Ply San, M.Map San Ply))
legalPliesWithMapping r = case (legalSansEither r, legalPliesEither r) of
  (Left _, Left _) -> mempty
  (Right sans, Right plies) ->
    let sMap :: M.Map Record San
        pMap :: M.Map Record Ply
        sMap = M.fromList $ fmap swap sans
        pMap = M.fromList $ fmap swap (toList plies)
        merged :: M.Map Record (San, Ply)
        merged =
          M.map unsafeConvert $
            M.merge
              (M.mapMaybeMissing $ \_k x -> Just (Just x, Nothing))
              (M.mapMaybeMissing $ \_k y -> Just (Nothing, Just y))
              (M.zipWithMatched $ \_k x y -> (Just x, Just y))
              sMap
              pMap
          where
            unsafeConvert = \case
              (Just s, Just p) -> (s, p)
              v -> error $ "some parts are missing: " <> show v
        pairs :: [(San, Ply)]
        pairs = M.elems merged
     in if M.size sMap /= M.size pMap || length sans /= M.size sMap
          then error $ "length mismatched: " <> show (length sans, M.size sMap, M.size pMap)
          else (M.fromList (toList plies), (M.fromList (fmap swap pairs), M.fromList pairs))
  _ -> error "result is inconsistent"

_todo :: a
_todo = error "TODO"

legalSansWithMapping :: Record -> (M.Map Ply Record, (M.Map Ply San, M.Map San Ply))
legalSansWithMapping r = (lm, _todo)
  where
    lm = legalPliesMap r

type PlyRec = (Ply, Record)

type SanRec = (San, Record)

{-
  Either gives all possible next plies, or
  conclude the game according to rules.

  Implementation notes:

  - Castle and Pawn promotion are special cases and are dealt with
    as the first step.

  - otherwise we try different approaches of disambiguation
    according to SAN spec.

  TODO: separate adjudication logic.

  TODO: make sure to only give GameResult if there are no moves available.

  As
  - this makes it a bit easier if we have to convert resulting value to a list.
  - we make it more clear that we don't deal with 3-fold reptition or other forms of draws,
    for lack of information.

  For this it is probably better that the resulting type is

  > Either GameResult (NonEmpty (San, Record))

 -}
legalSansEither :: Record -> Either GameResult [(San, Record)]
legalSansEither r@Record {placement} = convert . toList <$> legalPliesEither r
  where
    {-
      TODO: quick and dirty for now.
     -}

    {-
      Plies are separated into 3 categories:
      - castles
      - pawn promotions
      - everything else

      and each of which are handled in a particular way.
     -}
    partitionPlies ::
      [PlyRec] ->
      ( ( {- all castle plies -}
          [SanRec]
        , {- all pawn promotion plies -}
          [PlyRec]
        )
      , {- all of the rest -}
        [PlyRec]
      )
    partitionPlies xs = first mconcat $ partitionEithers (go <$> xs)
      where
        go pr@(p, _) =
          if
              | Just sr <- castlePlyToSan pr -> Left ([sr], mempty)
              | PlyPromo {} <- p -> Left (mempty, [pr])
              | otherwise -> Right pr

    simpleConvert :: Maybe Disamb -> PlyRec -> (San, Record)
    simpleConvert sFromPre (p, r') =
      ( SNorm
          { sPieceFrom = let Just (_, pt) = at placement (pFrom p) in pt
          , sFrom =
              sFromPre <|> do
                {-
                  Dispite that disambiguation on pawn moves are not always necessary,
                  PGN spec states that:

                  ... pawn captures include the file letter of the originating square
                  of the capturing pawn immediately prior to the "x" character.

                  So we need to make sure this disambiguation is here.

                  In addition, note the fact that only disambiguation by file is possible
                  on pawn moves. This is because all SAN includes target location (except for castle moves),
                  which implies source rank for pawn (since pawn can only move forward).
                  (double forward requires two squares in front of it being empty, therefore,
                  two pawn moves to the same target, say e3-e4 and e2-e4, cannot be possible at the same time)
                  This fact implies that it is unnecessary to include ranks in disambiguation of pawn moves,
                  excluding disambiguation by coord / rank.
                  -}
                (Pawn, _) <- pure (getPieceTypeCoord p)
                guard sCapture
                pure $ DisambByFile (coordFile (pFrom p))
          , sCapture = isCapturePly r p
          , sTo = pTo p
          , sPromo = case p of
              PlyNorm {} -> Nothing
              PlyPromo {pPiece} -> Just pPiece
          , sCheck = getCheckType r'
          }
      , r'
      )
      where
        sCapture = isCapturePly r p

    convert :: [PlyRec] -> [(San, Record)]
    convert xs =
      castles <> promoPliesToSan pawnPromos
        <> fmap (uncurry simpleConvert) rs1
        <> fmap (uncurry simpleConvert) rs2
        <> fmap (uncurry simpleConvert) rs3
        <> fmap (uncurry simpleConvert) rs4
      where
        {-
          Handle castle plies and pawn plies, after which
          we can deal with other types of plies left by `ls`.
         -}
        ((castles, pawnPromos), ls) = partitionPlies xs
        ls1 :: [[PlyRec]]
        rs1 :: [(Maybe Disamb, PlyRec)]
        (ls1, rs1) = partitionEithers $ fmap go $ M.toList $ performDisambBasic ls
          where
            go (_k, vs) = case vs of
              [] -> error "unreachable"
              [v] -> Right (Nothing, v)
              _ : _ : _ -> Left vs
        ls2 :: [[PlyRec]]
        (ls2, rs2) = partitionEithers $
          fmap go $ do
            M.toList $ performDisambByFile (concat ls1)
          where
            go ((_, fInd), vs) = case vs of
              [] -> error "unreachable"
              [v] -> Right (Just (DisambByFile fInd), v)
              _ : _ : _ -> Left vs

        ls3 :: [[PlyRec]]
        (ls3, rs3) = partitionEithers $
          fmap go $ do
            M.toList $ performDisambByRank (concat ls2)
          where
            go ((_, rInd), vs) = case vs of
              [] -> error "unreachable"
              [v] -> Right (Just (DisambByRank rInd), v)
              _ : _ : _ -> Left vs
        rs4 = (\v@(p, _) -> (Just (DisambByCoord (pFrom p)), v)) <$> concat ls3

    getPieceTypeCoord :: Ply -> (PieceType, Coord)
    getPieceTypeCoord p = let Just (_, pt) = at placement (pFrom p) in (pt, pTo p)

    coordRank, coordFile :: Coord -> Int
    coordRank c = withRankAndFile c (\rInd _fInd -> rInd)
    coordFile c = withRankAndFile c (\_rInd fInd -> fInd)
    performDisambBasic :: [PlyRec] -> M.Map (PieceType, Coord) [PlyRec]
    performDisambBasic xs = M.fromListWith (<>) $ do
      v@(p, _) <- xs
      pure (getPieceTypeCoord p, [v])
    performDisambByFile :: [PlyRec] -> M.Map ((PieceType, Coord), Int) [PlyRec]
    performDisambByFile xs =
      M.fromListWith (<>) $
        fmap (\v@(p, _) -> ((getPieceTypeCoord p, coordFile (pFrom p)), [v])) xs
    performDisambByRank :: [PlyRec] -> M.Map ((PieceType, Coord), Int) [PlyRec]
    performDisambByRank xs =
      M.fromListWith (<>) $
        fmap (\v@(p, _) -> ((getPieceTypeCoord p, coordRank (pFrom p)), [v])) xs

    castlePlyToSan :: PlyRec -> Maybe (San, Record)
    castlePlyToSan (p, r') = do
      s <- isCastlePly r p
      pure (SCastle s (getCheckType r'), r')

    promoPliesToSan :: [PlyRec] -> [SanRec]
    promoPliesToSan pp =
      fmap (\(p, md) -> simpleConvert md p) $
        fmap (,Nothing) noDisambs
          <> fmap (\pr@(p, _) -> (pr, Just (DisambByFile (coordFile (pFrom p))))) needDisambs
      where
        {-
          first group pawn promotes by (source file, target file).

          Since elements of each such group only differs in promotion target piece types,
          they don't need to be disambiguated against each other.

         -}
        promoGroups :: M.Map (Int, Int) [PlyRec]
        promoGroups =
          M.fromListWith (<>) $
            fmap (\pr@(p, _) -> ((coordFile (pFrom p), coordFile (pTo p)), [pr])) pp

        {-
          Basic disambiguation by file of the target coord.
         -}
        disambs :: M.Map Int [[PlyRec]]
        disambs = M.fromListWith (<>) $ (\((_srcF, tgtF), v) -> (tgtF, [v])) <$> M.toList promoGroups
        noDisambs, needDisambs :: [PlyRec]
        (concat -> noDisambs, concat . concat -> needDisambs) = partitionEithers $ go <$> M.elems disambs
          where
            go = \case
              [] -> error "unreachable"
              [v] -> Left v
              xs@(_ : _ : _) -> Right xs
