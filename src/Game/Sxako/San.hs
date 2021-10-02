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
import Game.Sxako.Coord
import Game.Sxako.Types

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
  deriving (Show, Eq)

data Disamb
  = DisambByFile Int
  | DisambByRank Int
  | DisambByCoord Coord
  deriving (Show, Eq)

data CheckType = Check | Checkmate
  deriving (Show, Eq)

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
      sFrom <- pure Nothing -- TODO
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
