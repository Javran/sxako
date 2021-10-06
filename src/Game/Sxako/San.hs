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
import Game.Sxako.Common
import Game.Sxako.Coord

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
