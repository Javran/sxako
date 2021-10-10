{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Game.Sxako.Fen
  ( Record (..)
  , fenP
  , initRecord
  , dragonRecord
  )
where

import Control.Applicative
import Control.Monad
import Data.Aeson
import Data.Attoparsec.ByteString.Char8 as Parser
import qualified Data.ByteString.Char8 as BSC
import Data.Char
import Data.Monoid
import Data.String
import qualified Data.Text as T
import Data.Text.Encoding
import Data.Word
import Game.Sxako.Board
import Game.Sxako.Castling
import Game.Sxako.Common
import Game.Sxako.Coord

{-

  Reference:
  - https://en.wikipedia.org/wiki/Forsyth%E2%80%93Edwards_Notation
  - https://ia902908.us.archive.org/26/items/pgn-standard-1994-03-12/PGN_standard_1994-03-12.txt

  Notes on `enPassantTarget`: although the spec says en passant target needs to be present
  even if taking is not possible, both Stockfish and Lichess are implementing them
  in a way such that the target is present only when taking is possible.
  Here we lean towards common practice and do not set en passant target
  if taking is not possible.

 -}

data Record = Record
  { placement :: Board
  , activeColor :: Color
  , castling :: Castling
  , enPassantTarget :: Maybe Coord
  , halfMove :: Int
  , fullMove :: Int
  }
  deriving (Eq, Ord)

instance FromJSON Record where
  parseJSON = withText "FEN" $ \t ->
    case parseOnly fenP (encodeUtf8 t) of
      Left msg -> fail msg
      Right r -> pure r

instance ToJSON Record where
  toJSON r = String (T.pack $ show r)

encodeFen :: Record -> String
encodeFen
  Record
    { placement
    , activeColor
    , castling
    , enPassantTarget
    , halfMove
    , fullMove
    } =
    unwords
      [ show placement
      , case activeColor of
          White -> "w"
          Black -> "b"
      , show castling
      , maybe "-" show enPassantTarget
      , show halfMove
      , show fullMove
      ]

instance Show Record where
  show = encodeFen

instance Read Record where
  readsPrec _ = readsByAttoparsecChar8 fenP

rawStandardBoard, rawDragonBoard :: IsString s => s
rawStandardBoard = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"
rawDragonBoard = "r1bqkbnr/pp1ppp1p/2n3p1/8/3NP3/8/PPP2PPP/RNBQKB1R w KQkq - 0 5"

initRecord, dragonRecord :: Record
Right initRecord = parseOnly fenP rawStandardBoard
Right dragonRecord = parseOnly fenP rawDragonBoard

pElemP :: Parser (Sum Word8, [Square])
pElemP = pieceP <|> emptiesP
  where
    pieceP = do
      ch <- peekChar'
      guard $ not (Parser.isDigit ch)
      Just p <- pure (charToPiece ch)
      _ <- anyChar
      pure (1, [Just p])
    emptiesP = do
      c <- satisfy (\ch -> ch >= '1' && ch <= '8')
      let cnt = ord c - ord '0'
      pure (Sum (fromIntegral cnt), replicate cnt Nothing)

{-
  Parsing a rank, should produce exactly 8 elements.
 -}
rankP :: Parser [Square]
rankP = do
  (Sum 8, es) <- mconcat <$> many1 pElemP
  pure es

placement2dP :: Parser Placement2D
placement2dP =
  (:) <$> rankP <*> replicateM 7 (char '/' *> rankP)

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
      rCh <- char '3' <|> char '6'
      pure (read [fCh, rCh])

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
