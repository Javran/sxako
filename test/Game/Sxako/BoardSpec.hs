module Game.Sxako.BoardSpec where

import Game.Sxako.Board
import Game.Sxako.Coord
import Game.Sxako.TestBoard
import Game.Sxako.Types
import Test.Hspec

spec :: Spec
spec = describe "setBoardAt" $ do
  let TestBoard b =
        read
          "__r_k__r|\
          \___p_p__|\
          \pq__p__p|\
          \____P_p_|\
          \__P__n__|\
          \___Q_PB_|\
          \__P__P_P|\
          \R____RK_"
  specify "set / clear White Rook at a1" $ do
    setBoardAt (White, Rook) a1 True b `shouldBe` b
    TestBoard (setBoardAt (White, Rook) a1 False b)
      `shouldBe` read
        "__r_k__r|\
        \___p_p__|\
        \pq__p__p|\
        \____P_p_|\
        \__P__n__|\
        \___Q_PB_|\
        \__P__P_P|\
        \_____RK_"
  specify "set / clear Black Bishop at f6" $ do
    setBoardAt (Black, Bishop) f6 False b `shouldBe` b
    TestBoard (setBoardAt (Black, Bishop) f6 True b)
      `shouldBe` read
        "__r_k__r|\
        \___p_p__|\
        \pq__pb_p|\
        \____P_p_|\
        \__P__n__|\
        \___Q_PB_|\
        \__P__P_P|\
        \R____RK_"
