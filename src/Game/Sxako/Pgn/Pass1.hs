module Game.Sxako.Pgn.Pass1 where

import qualified Data.Map.Strict as M
import Data.Maybe
import Data.Tree
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

unreachable :: a
unreachable = error "unreachable"

parse :: forall a. [Simp a] -> Either String [Tree a]
parse = \case
  [] -> pure []
  Simp x0 : xs0 -> case x0 of
    Right _ -> Left "cannot start with RAVs"
    Left x1 -> do
      {-
        The key is to realize that whenever we see the following sequence
        (in which all letters are moves, `...` could be many moves omitted):

        A0 (B0 B1 ...) (C0 C1 ...) (D0 D1 ...) A1 ...
        ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

        consecutive RAVs can be groupped to the move right prior to that
        (marked by `{}`):

        {A0 (B0 B1 ...) (C0 C1 ...) (D0 D1 ...)} A1 ...

        w.l.o.g. we are assuming RAVs are non-empty.

        Now we are facing following variations
        (assuming main line is always the first):

        - A0 [A1 ...] (main line)
        - B0 [B1 ...]
        - C0 [C1 ...]
        - D0 [D1 ...]

        Note we can recurse those [ ... ] parts.

       -}
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
                  (Simp (Left _)) ->
                    -- not possible since it's removed by use of `span`.
                    unreachable
                  (Simp (Right [])) -> Nothing
                  (Simp (Right ys@(_ : _))) -> Just ys
              )
              rightsPre
      hdPns <- parse xs1
      {-
        TODO: in case PGN is ill-formed, we might choose to parse as much as we can
        instead of failing the whole thing, which is a non-goal for now.
       -}
      (rs :: [] [Tree a]) <- mapM parse rights
      {-
        flattening is actually straightforward, notice that
        the following encodes the same ply tree:

        - A B (C) (D) (E)
        - A B (C (D)) (E)
        - A B (C) (D (E))

        so probably just concat-ing them together is sufficient.

       -}
      pure $ Node x1 hdPns : concat rs

newtype PlyNode a = PlyNode (M.Map a (PlyNode a)) deriving (Show, Eq)

{-
  Organizes input variations together into a forest indexed by plies.
 -}
densify :: forall a. Ord a => [Tree a] -> PlyNode a
densify = PlyNode . M.map densify . aux
  where
    aux :: [Tree a] -> M.Map a [Tree a]
    aux = M.unionsWith (<>) . fmap (\(Node x xs) -> M.singleton x xs)
