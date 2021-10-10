{-# LANGUAGE DerivingVia #-}

module Game.Sxako.Bitboard
  ( Bitboard (..)
  )
where

import Control.Monad
import Data.Bits
import Data.List
import Data.List.Split
import Data.Word
import Text.ParserCombinators.ReadP

newtype Bitboard = Bitboard
  { getBitboard :: Word64
  }
  deriving (Eq, Bits, Bounded, Ord) via Word64

{-
  Show & Read instance:

  - follows FEN placement order.
  - set bit: '*', cleared bit: '_'.
  - '|' is separator, no surrounding.

 -}

instance Show Bitboard where
  show (Bitboard v) =
    intercalate "|" . reverse . chunksOf 8 $
      fmap (\i -> if testBit v i then '*' else '_') [0 .. 63]

instance Read Bitboard where
  readsPrec _ = readP_to_S $ do
    let bitP = (True <$ char '*') <++ (False <$ char '_')
        rankP = replicateM 8 bitP
    x <- rankP
    xs <- replicateM 7 (char '|' *> rankP)
    let bits = concat . reverse $ x : xs
    pure $
      Bitboard $
        foldr
          (\(i, b) r -> if b then setBit r i else r)
          0
          (zip [0 .. 63] bits)
