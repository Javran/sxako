{-# LANGUAGE OverloadedStrings #-}

module Game.Sxako.Fen
  ( Record (..)
  , Placement
  , fenP
  , initRecord
  , dragonRecord
  )
where

import Control.Applicative
import Control.Monad
import Data.Attoparsec.ByteString.Char8 as Parser
import Data.Bifunctor
import Data.Char
import Data.Containers.ListUtils
import Data.Monoid
import Data.String
import qualified Data.Vector.Fixed as VF
import Data.Word
import Game.Sxako.Coord
import Game.Sxako.Types

{-
  Reference:
  - https://en.wikipedia.org/wiki/Forsyth%E2%80%93Edwards_Notation
  - https://ia902908.us.archive.org/26/items/pgn-standard-1994-03-12/PGN_standard_1994-03-12.txt
 -}

type Placement = EightElems (EightElems (Maybe Piece))

data Record = Record
  { placement :: Placement
  , activeColor :: Color
  , castling :: ([Side], [Side]) -- TODO: probably just Set or a Word8?
  , enPassantTarget :: Maybe Coord
  , halfMove :: Int
  , fullMove :: Int
  }
  deriving (Show)

{-
  Information of one sqaure: empty or there's something on it.
 -}
type Square = Maybe Piece

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

placementP :: Parser Placement
placementP = do
  fs <- (:) <$> rankP <*> replicateM 7 (char '/' *> rankP)
  pure $ VF.fromList' fs

activeColorP :: Parser Color
activeColorP = (White <$ char 'w') <|> (Black <$ char 'b')

{-
  TODO: We can make the representation a lot more compact by using bitset.

  TODO: according to spec, this can only be:

  - either "-"
  - or "KQkq" with some missing chars (not all missing)

  We can write a stricter version.

 -}
castlingP :: Parser ([Side], [Side])
castlingP = bimap nubOrd nubOrd . mconcat <$> many1 chP
  where
    chP =
      choice
        [ ([KingSide], []) <$ char 'K'
        , ([QueenSide], []) <$ char 'Q'
        , ([], [KingSide]) <$ char 'k'
        , ([], [QueenSide]) <$ char 'q'
        ]

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
  Record <$> tok placementP
    <*> tok activeColorP
    <*> tok castlingP
    <*> tok enPassantTargetP
    <*> tok decimal
    <*> decimal
  where
    tok p = p <* char ' '
