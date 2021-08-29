{-# LANGUAGE DerivingVia #-}

module Game.Sxako.Bitboard
  ( Bitboard (..)
  )
where

import Data.Bits
import Data.List
import Data.List.Split
import Data.Word

newtype Bitboard = Bitboard
  { getBitboard :: Word64
  }
  deriving (Eq, Bits) via Word64

instance Show Bitboard where
  show (Bitboard v) =
    intercalate "|" . reverse . chunksOf 8 $
      fmap (\i -> if testBit v i then '*' else '_') [0 .. 63]
