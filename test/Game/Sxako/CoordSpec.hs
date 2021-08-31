module Game.Sxako.CoordSpec where

import Game.Sxako.Coord
import Test.Hspec

spec :: Spec
spec =
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
