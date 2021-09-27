{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE TypeFamilies #-}

module Game.Sxako.Board.Halfboard.Vector
  ( Halfboard
  , HalfboardImpl (..)
  )
where

import qualified Data.Vector as V
import qualified Data.Vector.Mutable as VM
import Game.Sxako.Bitboard
import Game.Sxako.Board.Halfboard.Types

data Vector

type Halfboard = Vec Vector Bitboard

instance HalfboardImpl Vector where
  data Vec Vector a = Vec (V.Vector a) deriving (Eq, Foldable)
  data MVec Vector s a = MVec (VM.MVector s a)

  empty = Vec (V.replicate 6 (Bitboard 0))
  at (Vec hb) pt = hb V.! fromEnum pt
  modifyBitboard f pt (Vec hb) = Vec $ V.unsafeUpd hb [(i, f (hb V.! i))]
    where
      i = fromEnum pt
  thaw (Vec x) = MVec <$> V.thaw x
  unsafeRead (MVec x) i = VM.unsafeRead x i
  unsafeWrite (MVec x) i v = VM.unsafeWrite x i v
  unsafeFreeze (MVec x) = Vec <$> V.unsafeFreeze x
