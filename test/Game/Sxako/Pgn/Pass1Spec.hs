module Game.Sxako.Pgn.Pass1Spec where

import Data.Tree
import Game.Sxako.Pgn.Pass1
import Test.Hspec

spec :: Spec
spec = do
  describe "parse" do
    specify "RAVs" do
      let inp :: [Simp Char]
          inp =
            [ l 'A'
            , l 'B'
            , l 'C'
            , r [l 'F', l 'G']
            , r []
            , r [l 'H', l 'I', l 'J', r [l 'K', l 'L', l 'M']]
            , l 'D'
            , l 'E'
            , r [l 'N', l 'O', r [l 'P']]
            , l 'Q'
            ]
            where
              l ch = Simp (Left ch)
              r xs = Simp (Right xs)
          expect :: Tree Char
          expect =
            n
              'A'
              [ n
                  'B'
                  [ n
                      'C'
                      [ n
                          'D'
                          [ n
                              'E'
                              [n 'Q' []]
                          , n 'N' [n 'O' [], n 'P' []]
                          ]
                      ]
                  , n 'F' [n 'G' []]
                  , n
                      'H'
                      [ n
                          'I'
                          [ n 'J' []
                          , n
                              'K'
                              [ n 'L' [n 'M' []]
                              ]
                          ]
                      ]
                  ]
              ]
            where
              n = Node
      parse inp `shouldBe` Right [expect]
