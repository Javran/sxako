module Game.Sxako.Pgn.Pass1 where

import Data.List
import Data.Maybe
import Game.Sxako.Pgn.Pass0
import Game.Sxako.San

newtype Simp a = Simp (Either a [Simp a]) deriving (Show)

{-
  `Simp San` is a simplified form of MovetextElem,
  keeping just sufficient info (Sans / plies) to
  reconstruct PGN games.
 -}

fromMovetextElem :: MovetextElem -> Maybe (Simp San)
fromMovetextElem = \case
  MtMoveNum {} -> Nothing
  MtSan s _ -> Just (Simp $ Left s)
  MtCommentary {} -> Nothing
  MtRav xs -> Just (Simp $ Right $ mapMaybe fromMovetextElem xs)

newtype PlyNode a = PN (a, [PlyNode a]) deriving (Show)

unreachable :: a
unreachable = error "unreachable"

parse :: forall a. [Simp a] -> Either String [PlyNode a]
parse = \case
  [] -> pure []
  Simp x0 : xs0 -> case x0 of
    Right _ -> Left "cannot start with RAV"
    Left x1 -> do
      let (rightsPre :: [Simp a], xs1) =
            span
              ( \case
                  (Simp (Right _)) -> True
                  (Simp (Left _)) -> False
              )
              xs0
          rights :: [] [Simp a]
          rights =
            mapMaybe
              ( \case
                  (Simp (Left _)) -> unreachable
                  (Simp (Right [])) -> Nothing
                  (Simp (Right ys@(_ : _))) -> Just ys
              )
              rightsPre
      {-
        - x1, then xs1
        - for r <- rights,
          + head of r, then tail of r
       -}
      hdPns <- parse xs1
      (rs :: [] [PlyNode a]) <- mapM parse rights
      {-
        flattening is actually straightforward, notice that
        the following encodes the same ply tree:

        - A B (C) (D) (E)
        - A B (C (D)) (E)
        - A B (C) (D (E))

        so probably just concat-ing them together is sufficient.

       -}
      pure $ PN (x1, hdPns) : concat rs

{-
  TODO: parse the following as a proof of concept

  A B C (F G) () (H I J (K L M)) D E (N O (P)) Q
 -}

example0 :: [Simp Char]
example0 =
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
