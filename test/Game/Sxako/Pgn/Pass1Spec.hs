module Game.Sxako.Pgn.Pass1Spec where

import qualified Data.Map.Strict as M
import Data.Tree
import Game.Sxako.Pgn.Pass1
import Test.Hspec

spec :: Spec
spec = do
  let l ch = Simp (Left ch)
      r xs = Simp (Right xs)
      n = Node

  describe "parse" do
    specify "single line" do
      parse [l 'A', l 'B', l 'C']
        `shouldBe` Right
          [ n 'A' [n 'B' [n 'C' []]]
          ]

    specify "alts from start" do
      parse [l 'A', r [l 'B'], r [l 'C', l 'D']]
        `shouldBe` Right
          [ n 'A' []
          , n 'B' []
          , n 'C' [n 'D' []]
          ]

    specify "same tree different reps" do
      {-
        Turns out the PGN represnetation of ply-tree is not unique.
        This unit test offers an example.
       -}
      let expect :: Tree Char
          expect =
            n
              'A'
              [ n 'B' []
              , n 'C' []
              , n 'D' []
              , n 'E' []
              ]
      parse
        [ l 'A'
        , l 'B'
        , r [l 'C']
        , r [l 'D']
        , r [l 'E']
        ]
        `shouldBe` Right [expect]

      parse
        [ l 'A'
        , l 'B'
        , r [l 'C', r [l 'D']]
        , r [l 'E']
        ]
        `shouldBe` Right [expect]

      parse
        [ l 'A'
        , l 'B'
        , r [l 'C']
        , r [l 'D', r [l 'E']]
        ]
        `shouldBe` Right [expect]

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
      parse inp `shouldBe` Right [expect]
  describe "densify" do
    specify "example" do
      let pn = PlyNode . M.fromList
          trees =
            [ n 'A' [n 'B' [n 'C' [], n 'D' [n 'E' []]]]
            , n 'B' [n 'E' [n 'A' []]]
            , n 'A' [n 'C' [n 'D' []]]
            , n 'A' [n 'B' [n 'D' [n 'G' []]]]
            , n 'B' [n 'H' []]
            , n 'I' [n 'J' [n 'K' []]]
            ]
      densify trees
        `shouldBe` pn
          [
            ( 'A'
            , pn
                [ ('B', pn [('C', pn []), ('D', pn [('E', pn []), ('G', pn [])])])
                , ('C', pn [('D', pn [])])
                ]
            )
          ,
            ( 'B'
            , pn
                [ ('E', pn [('A', pn [])])
                , ('H', pn [])
                ]
            )
          , ('I', pn [('J', pn [('K', pn [])])])
          ]
