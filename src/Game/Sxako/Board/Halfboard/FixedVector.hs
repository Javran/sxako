{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE TypeFamilies #-}

module Game.Sxako.Board.Halfboard.FixedVector
  ( Halfboard
  , HalfboardImpl (..)
  )
where

import Data.Functor.Identity
import qualified Data.Vector.Fixed as VF
import qualified Data.Vector.Fixed.Boxed as VFB
import qualified Data.Vector.Fixed.Mutable as VFM
import Game.Sxako.Bitboard
import Game.Sxako.Board.Halfboard.Types

type V = VFB.Vec 6

type MV = VFM.Mutable V

type Halfboard = Vec FixedVector Bitboard

data FixedVector

instance HalfboardImpl FixedVector where
  data Vec FixedVector a = VF (V a) deriving (Eq, Foldable)

  data MVec FixedVector s a = MVF (MV s a)

  empty = VF $ VF.replicate (Bitboard 0)
  at (VF hb) pt = hb VF.! fromEnum pt
  modifyBitboard f pt (VF hb) =
    VF $ runIdentity . VF.element (fromEnum pt) (pure . f) $ hb

  thaw (VF x) = MVF <$> VFM.thaw x
  unsafeRead (MVF x) i = VFM.unsafeRead x i
  unsafeWrite (MVF x) i v = VFM.unsafeWrite x i v
  unsafeFreeze (MVF x) = VF <$> VFM.unsafeFreeze x
