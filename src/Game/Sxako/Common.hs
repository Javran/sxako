module Game.Sxako.Common (
  EightElems,
  PieceType (..),
  Color (..),
  Piece,
  Side (..),
  Placement2D,
  Square,
  CheckType (..),
  universe,
  charToPiece,
  pieceToChar,
  opposite,
  readsByAttoparsecChar8,
  NFData,
  Generic,
) where

import Control.DeepSeq (NFData)
import Data.Attoparsec.ByteString.Char8 as Parser
import qualified Data.ByteString.Char8 as BSC
import qualified Data.Map.Strict as M
import Data.Tuple
import GHC.Generics (Generic)

{-
  TODO: we should probably get rid of fixed-vector usage.

  as profiling indicates that there's still a significant amount
  of allocation overhead in fixed-vectors, it might not be the best
  structure to use as parsing intermediates.

 -}

{-
  (Unchecked) a list containing exactly 8 elements.
 -}
type EightElems = []

{-
  pawns and kings might not be considered pieces,
  but let's not make it more complicated than needed.
 -}
data PieceType
  = Pawn
  | Knight
  | Bishop
  | Rook
  | Queen
  | King
  deriving (Enum, Eq, Ord, Bounded, Show, Generic)

instance NFData PieceType

data Color = White | Black deriving (Enum, Eq, Ord, Bounded, Show, Generic)

instance NFData Color

opposite :: Color -> Color
opposite = \case
  White -> Black
  Black -> White

type Piece = (Color, PieceType)

universe :: (Enum a, Bounded a) => [a]
universe = [minBound .. maxBound]

data Side = KingSide | QueenSide deriving (Eq, Ord, Show, Generic)

instance NFData Side

{-
  2D list of a Chess board - used for intermediate representations.
 -}
type Placement2D = EightElems (EightElems (Maybe Piece))

{-
  Information of one sqaure: empty or there's something on it.
 -}
type Square = Maybe Piece

charToPiece :: Char -> Maybe Piece
charToPiece = (d M.!?)
  where
    d = M.fromList pieceTable

pieceToChar :: Piece -> Char
pieceToChar = (d M.!)
  where
    d = M.fromList (fmap swap pieceTable)

pieceTable :: [(Char, Piece)]
pieceTable =
  concat
    [ "Pp" <~> Pawn
    , "Nn" <~> Knight
    , "Bb" <~> Bishop
    , "Rr" <~> Rook
    , "Qq" <~> Queen
    , "Kk" <~> King
    ]
  where
    [wCh, bCh] <~> p = [(wCh, (White, p)), (bCh, (Black, p))]
    _ <~> _ = error "unreachable"

readsByAttoparsecChar8 :: Parser a -> ReadS a
readsByAttoparsecChar8 parser raw =
  case parseOnly ((,) <$> parser <*> takeByteString) (BSC.pack raw) of
    Left msg -> fail msg
    Right (r, left) -> [(r, BSC.unpack left)]

data CheckType = Check | Checkmate
  deriving (Show, Eq, Ord, Generic)

instance NFData CheckType
