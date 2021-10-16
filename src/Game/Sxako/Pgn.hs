module Game.Sxako.Pgn
  (
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

import Data.Attoparsec.ByteString.Char8 as Parser
import qualified Data.ByteString.Builder as Builder
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Text as T
import Data.Text.Encoding
import Control.Applicative

type TagPair = (T.Text, T.Text)

todo :: a
todo = error "todo"

tok :: Parser a -> Parser a
tok = (<* skipSpace)

stringLitP :: Parser T.Text
stringLitP = tr <$> (char '"' *> many noEscapeChunk <* char '"')
  where
    tr :: [Builder.Builder] -> T.Text
    tr = decodeLatin1 . BSL.toStrict . Builder.toLazyByteString . mconcat
    noEscapeChunk = Builder.byteString <$> Parser.takeWhile1 (/= '\\')

tagPairP :: Parser TagPair
tagPairP =
  tok (char '[')
    *> ((,)
          <$> (decodeLatin1 <$> tok (Parser.takeWhile1 isTagSymbol))
            <*> tok stringLitP)
    <* tok (char ']')
  where
    -- A further restriction on tag names is that they are composed exclusively of letters, digits, and the underscore character.
    isTagSymbol ch = isAlpha_iso8859_15 ch || isDigit ch || ch == '_'
