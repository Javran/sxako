{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}

{-
  Coordinates on a Chess board.
 -}
module Game.Sxako.Coord
  ( Coord
  , unsafeFromRankAndFile
  , fromRankAndFile
  , withRankAndFile
  , allCoords
  , toBit
  , fenCoords
  , isDark
  , Dir (..)
  , nextCoord
  , nextCoords
  , testBoard
  {- ORMOLU_DISABLE -}
  , a1, b1, c1, d1, e1, f1, g1, h1
  , a2, b2, c2, d2, e2, f2, g2, h2
  , a3, b3, c3, d3, e3, f3, g3, h3
  , a4, b4, c4, d4, e4, f4, g4, h4
  , a5, b5, c5, d5, e5, f5, g5, h5
  , a6, b6, c6, d6, e6, f6, g6, h6
  , a7, b7, c7, d7, e7, f7, g7, h7
  , a8, b8, c8, d8, e8, f8, g8, h8
  {- ORMOLU_ENABLE -}
  )
where

import Control.Monad
import Data.Bits
import Data.Char
import Data.List
import Data.List.Split
import Data.Word
import Game.Sxako.Bitboard
import Text.ParserCombinators.ReadP

{-
  INVARIANT: Word8 but only takes the value 0~63 (inclusive).

  - low bits (0~2) represents file
  - high bits (3~5) represents rank
 -}
newtype Coord = Coord Word8 deriving (Eq)

{-
  Direction for Coords to move to next.

  This is defined from the perspective of white:

  - North: rank += 1
  - East: file += 1
  - South: rank -= 1
  - West: file -= 1

 -}
data Dir
  = DN
  | DNE
  | DE
  | DSE
  | DS
  | DSW
  | DW
  | DNW

nextCoord :: Dir -> Coord -> Maybe Coord
nextCoord d c = withRankAndFile @Int c $ \rInd fInd ->
  case d of
    DN -> fromRankAndFile (rInd + 1) fInd
    DNE -> fromRankAndFile (rInd + 1) (fInd + 1)
    DE -> fromRankAndFile rInd (fInd + 1)
    DSE -> fromRankAndFile (rInd -1) (fInd + 1)
    DS -> fromRankAndFile (rInd - 1) fInd
    DSW -> fromRankAndFile (rInd -1) (fInd - 1)
    DW -> fromRankAndFile rInd (fInd - 1)
    DNW -> fromRankAndFile (rInd + 1) (fInd -1)

nextCoords :: Dir -> Coord -> [Coord]
nextCoords d =
  unfoldr
    (\c -> do
       cNext <- nextCoord d c
       pure (cNext, cNext))

instance Show Coord where
  show c =
    withRankAndFile
      c
      $ \r f ->
        [chr (ord 'a' + f), chr (ord '1' + r)]

instance Read Coord where
  readsPrec _ = readP_to_S $ do
    fCh <- satisfy (\c -> c >= 'a' && c <= 'h')
    rCh <- satisfy (\c -> c >= '1' && c <= '8')
    pure $ unsafeFromRankAndFile (ord rCh - ord '1') (ord fCh - ord 'a')

instance Bounded Coord where
  minBound = a1
  maxBound = h8

instance Enum Coord where
  fromEnum (Coord i) = fromIntegral i
  toEnum i =
    if i >= 0 && i <= 63
      then Coord (fromIntegral i)
      else error "must be within 0~63."

{-
  Rank and file are both expected to be in [0..7]
 -}
unsafeFromRankAndFile :: Integral i => i -> i -> Coord
unsafeFromRankAndFile
  (fromIntegral -> rInd)
  (fromIntegral -> fInd) =
    Coord $ shiftL rInd 3 .|. fInd

fromRankAndFile :: Integral i => i -> i -> Maybe Coord
fromRankAndFile r f =
  unsafeFromRankAndFile r f
    <$ guard (r >= 0 && r < 8 && f >= 0 && f < 8)

withRankAndFile :: Integral i => Coord -> (i -> i -> r) -> r
withRankAndFile (Coord c) action =
  action (fromIntegral r) (fromIntegral f)
  where
    r = shiftR c 3
    f = c .&. 7

isDark :: Coord -> Bool
isDark c = withRankAndFile c (\r f -> even (r + f :: Word8))

{-
  Produces a bitboard with only the specified coord set.
 -}
toBit :: Coord -> Word64
toBit (Coord c) = bit (fromIntegral c)

fenCoords :: [[Coord]]
fenCoords = reverse (chunksOf 8 allCoords)

testBoard :: Bitboard -> Coord -> Bool
testBoard bb (Coord c) = testBit bb (fromIntegral c)

_gen :: IO ()
_gen = do
  let coords = do
        rank <- ['1' .. '8']
        file <- ['a' .. 'h']
        pure [file, rank]
      comSepCoords = intercalate ", " coords
  putStrLn $ comSepCoords <> " :: Coord"
  putStrLn $ "[" <> comSepCoords <> "] = fmap Coord [0..63]"

allCoords :: [Coord]
{-
  The section below are generated by `_gen`.
 -}
a1, b1, c1, d1, e1, f1, g1, h1, a2, b2, c2, d2, e2, f2, g2, h2, a3, b3, c3, d3, e3, f3, g3, h3, a4, b4, c4, d4, e4, f4, g4, h4, a5, b5, c5, d5, e5, f5, g5, h5, a6, b6, c6, d6, e6, f6, g6, h6, a7, b7, c7, d7, e7, f7, g7, h7, a8, b8, c8, d8, e8, f8, g8, h8 :: Coord
allCoords@[a1, b1, c1, d1, e1, f1, g1, h1, a2, b2, c2, d2, e2, f2, g2, h2, a3, b3, c3, d3, e3, f3, g3, h3, a4, b4, c4, d4, e4, f4, g4, h4, a5, b5, c5, d5, e5, f5, g5, h5, a6, b6, c6, d6, e6, f6, g6, h6, a7, b7, c7, d7, e7, f7, g7, h7, a8, b8, c8, d8, e8, f8, g8, h8] = fmap Coord [0 .. 63]
