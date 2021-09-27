{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Game.Sxako.Board.Halfboard
  ( Halfboard
  , emptyHb
  , hbAt
  , modifyBitboard
  , hbThaw
  , hbUnsafeRead
  , hbUnsafeWrite
  , hbUnsafeFreeze
  )
where

import Control.Monad.Primitive
import Data.Functor.Identity
import qualified Data.Vector.Fixed as VF
import qualified Data.Vector.Fixed.Boxed as VFB
import qualified Data.Vector.Fixed.Mutable as VFM
import Game.Sxako.Bitboard
import Game.Sxako.Types

type V = VFB.Vec 6

type MV = VFM.Mutable V

type Halfboard = V Bitboard

emptyHb :: Halfboard
emptyHb = VF.replicate (Bitboard 0)

hbAt :: Halfboard -> PieceType -> Bitboard
hbAt hb pt = hb VF.! fromEnum pt

modifyBitboard :: (Bitboard -> Bitboard) -> PieceType -> Halfboard -> Halfboard
modifyBitboard f pt =
  runIdentity . VF.element (fromEnum pt) (pure . f)

hbThaw :: PrimMonad m => V a -> m (MV (PrimState m) a)
hbThaw = VFM.thaw

hbUnsafeRead :: PrimMonad m => MV (PrimState m) a -> Int -> m a
hbUnsafeRead = VFM.unsafeRead

hbUnsafeWrite :: PrimMonad m => MV (PrimState m) a -> Int -> a -> m ()
hbUnsafeWrite = VFM.unsafeWrite

hbUnsafeFreeze :: PrimMonad m => MV (PrimState m) a -> m (V a)
hbUnsafeFreeze = VFM.unsafeFreeze
