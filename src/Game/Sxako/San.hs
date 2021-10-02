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
import Data.Attoparsec.ByteString.Char8 as Parser
import Game.Sxako.Coord
import Game.Sxako.Move
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

sanP :: Parser San
sanP = castleP
  where
    castleP = do
      _ <- string "O-O"
      mCh <- peekChar
      sSide <- case mCh of
        Just '-' -> QueenSide <$ string "-O"
        _ -> pure KingSide
      sCheck <-
        option Nothing $
          Just
            <$> ((Check <$ char '+')
                   <|> (Checkmate <$ char '#'))
      pure SCastle {sSide, sCheck}
