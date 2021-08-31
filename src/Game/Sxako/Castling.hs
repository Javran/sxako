{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE LambdaCase #-}

{-
  A simple bitset representing the availability of castling.
 -}

module Game.Sxako.Castling
  ( Castling (..)
  , none
  , whiteKingSide
  , whiteQueenSide
  , blackKingSide
  , blackQueenSide
  , canCastle
  )
where

import Data.Bits
import Data.Word
import Game.Sxako.Types
import Text.ParserCombinators.ReadP

newtype Castling = Castling Word8
  deriving (Eq, Bits) via Word8

none, whiteKingSide, whiteQueenSide, blackKingSide, blackQueenSide :: Castling
[ none
  , whiteKingSide
  , whiteQueenSide
  , blackKingSide
  , blackQueenSide
  ] = fmap Castling [0, 1, 2, 4, 8]

bitmask :: Color -> Side -> Castling
bitmask c s = case (c, s) of
  (White, KingSide) -> whiteKingSide
  (White, QueenSide) -> whiteQueenSide
  (Black, KingSide) -> blackKingSide
  (Black, QueenSide) -> blackQueenSide

canCastle :: Castling -> Color -> Side -> Bool
canCastle v c s = v .&. bitmask c s /= none

instance Show Castling where
  show c =
    if c == none
      then "-"
      else
        let m ?=> ch =
              if m .&. c /= none
                then (ch :)
                else id
         in (whiteKingSide ?=> 'K')
              . (whiteQueenSide ?=> 'Q')
              . (blackKingSide ?=> 'k')
              . (blackQueenSide ?=> 'q')
              $ ""

instance Read Castling where
  readsPrec _ =
    readP_to_S $
      (none <$ char '-')
        <++ (foldr (.|.) none
               <$> sequence
                 (let ch ?=> m =
                        look >>= \case
                          c : _ | c == ch -> m <$ get
                          _ -> pure none
                   in [ 'K' ?=> whiteKingSide
                      , 'Q' ?=> whiteQueenSide
                      , 'k' ?=> blackKingSide
                      , 'q' ?=> blackQueenSide
                      ]))
