{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Game.Sxako.San
  ( San (..)
  , Disamb (..)
  , CheckType (..)
  , sanP
  )
where

{-
  Short Algebraic Notation
 -}

import Control.Applicative
import Control.Monad
import Data.Attoparsec.ByteString.Char8 as Parser
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

{-
  TODO: impl idea:

  (1) deal with special situations:
    + convert castle plies first
    + pawn moves always disamb by file
  (2) after (1) is done, we only have disamb to worry about:
    + group by (PieceType, pTo)
    + done if a groupping contains exactly one.
    + otherwise try disamb by file, by rank, then fallback to coord.

 -}
legalSansEither :: Record -> Either GameResult [(San, Record)]
legalSansEither r@Record {placement} = convert <$> legalPliesEither r
  where
    simpleConvert :: Maybe Disamb -> (Ply, Record) -> (San, Record)
    simpleConvert sFrom (p, r') =
      ( SNorm
          { sPieceFrom = undefined
          , sFrom
          , sCapture = isCapturePly r p
          , sTo = pTo p
          , sPromo = Nothing
          , sCheck = getCheckType r'
          }
      , r'
      )

    convert :: [(Ply, Record)] -> [(San, Record)]
    convert xs =
      rs
        <> fmap (uncurry simpleConvert) rs1
        <> undefined
      where
        {-
          Handle castle plies and pawn plies, after which
          we can deal with other types of plies left by `ls`.
         -}
        (ls, rs) = partitionEithers (fmap handleSpecialPlies xs)
        ls1 :: [[(Ply, Record)]]
        rs1 :: [(Maybe Disamb, (Ply, Record))]
        (ls1, rs1) = partitionEithers $ fmap go $ M.toList $ performDisambBasic ls
          where
            go (_k, vs) = case vs of
              [] -> error "unreachable"
              [v] -> Right (Nothing, v)
              _ : _ : _ -> Left vs

    performDisambBasic :: [(Ply, Record)] -> M.Map (PieceType, Coord) [(Ply, Record)]
    performDisambBasic xs = M.fromListWith (<>) $ do
      v@(p, _) <- xs
      let Just (_, pt) = at placement (pFrom p)
      pure ((pt, pTo p), [v])
    performDisambByFile :: [(Ply, Record)] -> M.Map Int [(Ply, Record)]
    performDisambByFile xs =
      M.fromListWith (<>) $
        fmap (\v@(p, _) -> (withRankAndFile (pFrom p) (\_rInd fInd -> fInd), [v])) xs
    performDisambByRank :: [(Ply, Record)] -> M.Map Int [(Ply, Record)]
    performDisambByRank xs =
      M.fromListWith (<>) $
        fmap (\v@(p, _) -> (withRankAndFile (pFrom p) (\rInd _fInd -> rInd), [v])) xs

    handleSpecialPlies :: (Ply, Record) -> Either (Ply, Record) (San, Record)
    handleSpecialPlies a = case castlePlyToSan a of
      Nothing -> case pawnPlyToSan a of
        Nothing -> Left a
        Just b -> Right b
      Just b -> Right b

    castlePlyToSan :: (Ply, Record) -> Maybe (San, Record)
    castlePlyToSan (p, r') = do
      s <- isCastlePly r p
      pure (SCastle s (getCheckType r'), r')
    pawnPlyToSan :: (Ply, Record) -> Maybe (San, Record)
    pawnPlyToSan (p, r') = do
      (_, Pawn) <- at placement (pFrom p)
      pure
        ( SNorm
            { sPieceFrom = Pawn
            , sFrom =
                Just
                  (DisambByFile
                     (withRankAndFile (pFrom p) (\_r f -> f)))
            , sCapture = isCapturePly r p
            , sTo = pTo p
            , sPromo = case p of
                PlyNorm {} -> Nothing
                PlyPromo {pPiece} -> Just pPiece
            , sCheck = getCheckType r'
            }
        , r'
        )
