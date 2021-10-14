{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns #-}

module Game.Sxako.San
  ( San (..)
  , Disamb (..)
  , CheckType (..)
  , sanP
  , legalSansEither
  )
where

{-
  Short Algebraic Notation
 -}

import Control.Applicative
import Control.Monad
import Data.Attoparsec.ByteString.Char8 as Parser
import Data.Bifunctor
import Data.Char
import Data.Either
import qualified Data.Map.Strict as M
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
  deriving (Eq, Ord)

data Disamb
  = DisambByFile Int
  | DisambByRank Int
  | DisambByCoord Coord
  deriving (Show, Eq, Ord)

instance Read San where
  readsPrec _ = readsByAttoparsecChar8 sanP

instance Show San where
  show x =
    (case x of
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
           QueenSide -> "O-O-O")
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
      <$> ((Check <$ char '+')
             <|> (Checkmate <$ char '#'))

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
 -}
legalSansEither :: Record -> Either GameResult [(San, Record)]
legalSansEither r@Record {placement} = convert <$> legalPliesEither r
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
    partitionPlies
      :: [PlyRec]
      -> ( ( {- all castle plies -}
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
