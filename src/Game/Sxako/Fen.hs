{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Game.Sxako.Fen
  ( Record (..)
  , Placement2D
  , fenP
  , initRecord
  , dragonRecord
  )
where

import Control.Applicative
import Control.Monad
import Data.Attoparsec.ByteString.Char8 as Parser
import qualified Data.ByteString.Char8 as BSC
import Data.Char
import Data.Monoid
import Data.String
import qualified Data.Vector.Fixed as VF
import Data.Word
import Game.Sxako.Castling
import Game.Sxako.Coord
import Game.Sxako.Types
import Game.Sxako.Board

{-
  Reference:
  - https://en.wikipedia.org/wiki/Forsyth%E2%80%93Edwards_Notation
  - https://ia902908.us.archive.org/26/items/pgn-standard-1994-03-12/PGN_standard_1994-03-12.txt
 -}

data Record = Record
  { placement :: Board
  , activeColor :: Color
  , castling :: Castling
  , enPassantTarget :: Maybe Coord
  , halfMove :: Int
  , fullMove :: Int
  }
  deriving (Show)

rawStandardBoard, rawDragonBoard :: IsString s => s
rawStandardBoard = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"
rawDragonBoard = "r1bqkbnr/pp1ppp1p/2n3p1/8/3NP3/8/PPP2PPP/RNBQKB1R w KQkq - 0 5"

initRecord, dragonRecord :: Record
Right initRecord = parseOnly fenP rawStandardBoard
Right dragonRecord = parseOnly fenP rawDragonBoard

pElemP :: Parser (Sum Word8, [Square])
pElemP =
  choice
    [ "Pp" ~> Pawn
    , "Nn" ~> Knight
    , "Bb" ~> Bishop
    , "Rr" ~> Rook
    , "Qq" ~> Queen
    , "Kk" ~> King
    , do
        c <- satisfy (\ch -> ch >= '1' && ch <= '8')
        let cnt = ord c - ord '0'
        pure (Sum (fromIntegral cnt), replicate cnt Nothing)
    ]
  where
    [wRaw, bRaw] ~> pt = do
      color <-
        (White <$ char wRaw)
          <|> (Black <$ char bRaw)
      pure (1, [Just (color, pt)])
    _ ~> _ = error "unreachable"

{-
  Parsing a rank, should produce exactly 8 elements.
 -}
rankP :: Parser (EightElems Square)
rankP = do
  (Sum 8, es) <- mconcat <$> many1 pElemP
  pure $ VF.fromList' es

placement2dP :: Parser Placement2D
placement2dP = do
  fs <- (:) <$> rankP <*> replicateM 7 (char '/' *> rankP)
  pure $ VF.fromList' fs

activeColorP :: Parser Color
activeColorP = (White <$ char 'w') <|> (Black <$ char 'b')

castlingP :: Parser Castling
castlingP = do
  -- just grab next non-space chunk and delegate parsing to its Read instance.
  raw <- BSC.unpack <$> Parser.takeWhile1 (/= ' ')
  [(r, "")] <- pure $ reads @Castling raw
  pure r

enPassantTargetP :: Parser (Maybe Coord)
enPassantTargetP = (Nothing <$ char '-') <|> Just <$> enPassantSquareP
  where
    enPassantSquareP = do
      fCh <- satisfy (\ch -> ch >= 'a' && ch <= 'h')
      let f = ord fCh - ord 'a'
      r <- 3 <$ char '3' <|> 6 <$ char '6'
      pure (unsafeFromRankAndFile r f)

fenP :: Parser Record
fenP =
  Record <$> tok (fromPlacement2d <$> placement2dP)
    <*> tok activeColorP
    <*> tok castlingP
    <*> tok enPassantTargetP
    <*> tok decimal
    <*> decimal
  where
    tok p = p <* char ' '
