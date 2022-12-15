module Game.Sxako.Pgn.Pass1 where

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
