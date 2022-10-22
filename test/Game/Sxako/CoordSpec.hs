module Game.Sxako.CoordSpec where

import Control.Monad
import Data.Bits
import Game.Sxako.Common
import Game.Sxako.Coord
import Test.Hspec

spec :: Spec
spec = do
  describe "Coord" $
    specify "Read & Show instances" $ do
      forM_ (universe @Coord) $ \c ->
        read (show c) `shouldBe` c
  describe "nextCorods" $ do
    specify "e3, all directions" $ do
      let d ~> result = nextCoords d e3 `shouldBe` result
      DN ~> [e4, e5, e6, e7, e8]
      DNE ~> [f4, g5, h6]
      DE ~> [f3, g3, h3]
      DSE ~> [f2, g1]
      DS ~> [e2, e1]
      DSW ~> [d2, c1]
      DW ~> [d3, c3, b3, a3]
      DNW ~> [d4, c5, b6, a7]
  describe "toBit" $
    specify "examples" $ do
      toBit a1 `shouldBe` bit 0
      toBit h1 `shouldBe` bit 7
      toBit h8 `shouldBe` bit 63
