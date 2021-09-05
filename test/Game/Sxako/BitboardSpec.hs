{-# LANGUAGE NumericUnderscores #-}

module Game.Sxako.BitboardSpec where

import Game.Sxako.Bitboard
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
import Test.QuickCheck.Gen

spec :: Spec
spec =
  describe "Bitboard" $ do
    specify "simple" $
      read
        "________|\
        \________|\
        \________|\
        \________|\
        \________|\
        \________|\
        \________|\
        \_*______"
        `shouldBe` Bitboard 2
    specify "Hadamard matrix" $
      read
        "***_*___|\
        \**___*_*|\
        \*_**___*|\
        \*_*__**_|\
        \*___*_**|\
        \**_*__*_|\
        \*__***__|\
        \********"
        `shouldBe`
        {-
          - Every two hex num is a rank,
            with most siginificant digits being rank 8.
          - Within each rank, file goes from a to h,
            with least significant bit representing a-file.
         -}
        Bitboard 0x17_A3_8D_65_D1_4B_39_FF
    prop "Read & Show instance" $ do
      v <- chooseWord64 (minBound, maxBound)
      let b = Bitboard v
      pure $ read (show b) === b
