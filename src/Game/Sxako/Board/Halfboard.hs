module Game.Sxako.Board.Halfboard (
  Halfboard,
  empty,
  at,
  modifyBitboard,
  thaw,
  unsafeRead,
  unsafeWrite,
  unsafeFreeze,
) where

import Control.Monad.Primitive
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as VM
import Game.Sxako.Bitboard
import Game.Sxako.Common

type Halfboard = V.Vector Bitboard

empty :: V.Vector Bitboard
empty = V.replicate 6 (Bitboard 0)

at :: V.Vector a -> PieceType -> a
at hb pt = hb V.! fromEnum pt

modifyBitboard :: (a -> a) -> PieceType -> V.Vector a -> V.Vector a
modifyBitboard f pt hb = V.unsafeUpd hb [(i, f (hb V.! i))]
  where
    i = fromEnum pt

thaw :: PrimMonad m => V.Vector a -> m (VM.MVector (PrimState m) a)
thaw = V.thaw

unsafeRead :: PrimMonad m => VM.MVector (PrimState m) a -> Int -> m a
unsafeRead = VM.unsafeRead

unsafeWrite :: PrimMonad m => VM.MVector (PrimState m) a -> Int -> a -> m ()
unsafeWrite = VM.unsafeWrite

unsafeFreeze :: PrimMonad m => VM.MVector (PrimState m) a -> m (V.Vector a)
unsafeFreeze = V.unsafeFreeze
