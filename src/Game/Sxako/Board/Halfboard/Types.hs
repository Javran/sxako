{-# LANGUAGE TypeFamilies #-}

module Game.Sxako.Board.Halfboard.Types
  ( HalfboardImpl (..)
  )
where

import Control.Monad.Primitive
import Game.Sxako.Bitboard
import Game.Sxako.Types

class HalfboardImpl (vi :: *) where
  {-
    Using data family as injectivity is required.
   -}
  data Vec vi :: * -> *
  data MVec vi :: * -> * -> *

  empty :: Vec vi Bitboard
  at :: Vec vi Bitboard -> PieceType -> Bitboard
  modifyBitboard
    :: (Bitboard -> Bitboard)
    -> PieceType
    -> Vec vi Bitboard
    -> Vec vi Bitboard

  thaw :: PrimMonad m => Vec vi a -> m (MVec vi (PrimState m) a)
  unsafeRead :: PrimMonad m => MVec vi (PrimState m) a -> Int -> m a
  unsafeWrite :: PrimMonad m => MVec vi (PrimState m) a -> Int -> a -> m ()
  unsafeFreeze :: PrimMonad m => MVec vi (PrimState m) a -> m (Vec vi a)
