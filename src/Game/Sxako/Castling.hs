{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE LambdaCase #-}

{-
  A simple bitset representing the availability of castling.

  Show and Read instance of Castling follows FEN spec.

 -}

module Game.Sxako.Castling
  ( Castling (..)
  , none
  , whiteKingSide
  , whiteQueenSide
  , blackKingSide
  , blackQueenSide
  , allAllowed
  , canCastle
  )
where

import Data.Bits
import Data.Word
import Game.Sxako.Types
import Text.ParserCombinators.ReadP

newtype Castling = Castling Word8
  deriving (Eq, Enum, Bits) via Word8

none, whiteKingSide, whiteQueenSide, blackKingSide, blackQueenSide, allAllowed :: Castling
( [ none
    , whiteKingSide
    , whiteQueenSide
    , blackKingSide
    , blackQueenSide
    ]
  , allAllowed
  ) = (xs, foldr1 (.|.) xs)
    where
      xs = fmap Castling [0, 1, 2, 4, 8]

bitmask :: Color -> Side -> Castling
bitmask c s = case (c, s) of
  (White, KingSide) -> whiteKingSide
  (White, QueenSide) -> whiteQueenSide
  (Black, KingSide) -> blackKingSide
  (Black, QueenSide) -> blackQueenSide

canCastle :: Castling -> Color -> Side -> Bool
canCastle v c s = v .&. bitmask c s /= none

{-
  Ordering matters as we are respecting FEN castling notations.
 -}
symTable :: [(Char, Castling)]
symTable =
  [ ('K', whiteKingSide)
  , ('Q', whiteQueenSide)
  , ('k', blackKingSide)
  , ('q', blackQueenSide)
  ]

instance Bounded Castling where
  minBound = none
  maxBound = allAllowed

instance Show Castling where
  show c =
    if c == none
      then "-"
      else
        foldr
          (\(ch, m) r ->
             if m .&. c /= none
               then (ch :) . r
               else r)
          id
          symTable
          ""

instance Read Castling where
  readsPrec _ =
    readP_to_S $
      (none <$ char '-')
        <++ (foldr (.|.) none
               <$> mapM
                 (\(ch, m) ->
                    {-
                      using lookahead rather than optional
                      to avoid unwanted branches.
                     -}
                    look >>= \case
                      c : _ | c == ch -> m <$ get
                      _ -> pure none)
                 symTable)
