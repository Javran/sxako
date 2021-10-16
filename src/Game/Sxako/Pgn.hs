module Game.Sxako.Pgn
  ( tagPairP
  )
where

{-
  TODO: parsing PGN file.

  Reference: http://www.saremba.de/chessgml/standards/pgn/pgn-complete.htm
  - 18: Formal syntax

  Import and export format: it appears that import format is just a lax form
  of the export format, meaning that a parser accepting import format is sufficient
  to handle export format without changes.

  We might need to break parsing down into two steps:

  - parsing the file literally.
  - interpret movetext following along FEN.

  Movetext has the problem that it can only be interpreted
  properly given current FEN. So instead of making parsing stateful
  (in the sense that we "validate" a movetext as we replay
  from starting FEN), we tries to interpret the result
  only after parsing is done.

  For a FEN record, we generate all plies in `Ply` datatype,
  and then come up with another type mapping all plies
  to short algebratic notation.

  TODO: implementation plan.

  - first round to recognize tag pairs and movetext section, without verification
    + parsing tag pairs
    + parsing movetext
  - second round to turn this intermediate structure into a more organized one,
    verification included.

 -}

import Control.Applicative
import Data.Attoparsec.ByteString.Char8 as Parser
import qualified Data.ByteString.Builder as Builder
import qualified Data.ByteString.Lazy as BSL
import Data.Char
import qualified Data.Text as T
import Data.Text.Encoding

type TagPair = (T.Text, T.Text)

todo :: a
todo = error "todo"

-- TODO: how to recognize end of tag pair section if we are using skipSpace?
tok :: Parser a -> Parser a
tok = (<* skipSpace)

stringLitP :: Parser T.Text
stringLitP =
  tr
    <$> (char '"'
           *> many
             (noEscapeChunk
                <|> (charToBuilder <$> escapedChar))
             <* char '"')
  where
    tr :: [Builder.Builder] -> T.Text
    tr =
      decodeLatin1
        . BSL.toStrict
        . Builder.toLazyByteString
        . mconcat

    {-
      A quote inside a string is represented by the backslash immediately followed by a quote.
      A backslash inside a string is represented by two adjacent backslashes.
     -}
    noEscapeChunk =
      Builder.byteString
        <$> Parser.takeWhile1 (/= '\\')
    charToBuilder :: Char -> Builder.Builder
    charToBuilder = Builder.int8 . fromIntegral . ord
    escapedChar =
      char '\\'
        *> (('"' <$ char '"') <|> ('\\' <$ char '\\'))

tagPairP :: Parser TagPair
tagPairP =
  tok (char '[')
    *> ((,)
          <$> (decodeLatin1 <$> tok (Parser.takeWhile1 isTagSymbol))
            <*> tok stringLitP)
    <* tok (char ']')
  where
    {-
      A further restriction on tag names is that they are composed exclusively of
      letters, digits, and the underscore character.
     -}
    isTagSymbol ch = isAlpha_iso8859_15 ch || Parser.isDigit ch || ch == '_'
