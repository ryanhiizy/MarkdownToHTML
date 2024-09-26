module Assignment (markdownParser) where

import Control.Applicative (Alternative (..), Applicative (liftA2), asum, liftA3)
import Control.Monad (guard)
import Data.Time.Clock (getCurrentTime)
import Data.Time.Format (defaultTimeLocale, formatTime)
import Instances (Parser (..))
import Parser (between, betweenCode, charTok, freeText', getContent, hashLength, headingSepLength, inlineSpace, is, isNotWhitespace, isPositiveInt, space, string, unexpectedStringParser, untilChar, untilIncludingChar, untilIncludingCharTok)

data ADT
  = Italic String
  | Bold String
  | Strikethrough String
  | Link String String
  | InlineCode String
  | Footnote Int
  | Image String String String
  | FootnoteReference ADT String
  | FreeText String
  | Heading Int [ADT]
  | Blockquote [ADT]
  | Code String
  | Item [ADT]
  | OrderedList [ADT]
  | NewLine Char
  | Text [ADT]
  | Paragraph [ADT]
  deriving (Show, Eq)

italic :: Parser ADT
italic = Italic <$> between "_"

bold :: Parser ADT
bold = Bold <$> between "**"

strikethrough :: Parser ADT
strikethrough = Strikethrough <$> between "~~"

link :: Parser ADT
link = liftA2 Link (is '[' *> untilIncludingCharTok ']') (is '(' *> untilIncludingChar ')')

inlineCode :: Parser ADT
inlineCode = InlineCode <$> between "`"

footnote :: Parser ADT
footnote = Footnote <$> (string "[^" *> isNotWhitespace *> isPositiveInt <* is ']')

image :: Parser ADT
image =
  liftA3
    Image
    (string "![" *> untilIncludingCharTok ']')
    (is '(' *> getContent " \"")
    (untilIncludingCharTok '"' <* is ')')

footnoteReference :: Parser ADT
footnoteReference = liftA2 FootnoteReference footnote (charTok ':' *> untilChar '\n')

freeText :: Parser ADT
freeText = FreeText <$> some freeText'

heading :: Parser ADT
heading = liftA2 Heading hashLength (space *> line) <|> flip Heading <$> (line <* is '\n') <*> headingSepLength

blockquote :: Parser ADT
blockquote = Blockquote <$> (inlineSpace *> charTok '>' *> line)

code :: Parser ADT
code = Code <$> betweenCode "```"

nestedList :: Int -> Parser ADT
nestedList n = string (replicate (n + 4) ' ') *> orderedList' (n + 4)

-- need to check > 1?
listContent :: Int -> Parser ADT
listContent n = do
  indent <- length <$> inlineSpace
  guard (indent == n) <|> unexpectedStringParser "Indentation must be equal to the current level"
  _ <- isPositiveInt *> string ". "
  Item <$> line

list :: Int -> Parser ADT
list n = is '\n' *> (nestedList n <|> listContent n)

orderedList' :: Int -> Parser ADT
orderedList' n = do
  number <- isPositiveInt
  guard (number == 1) <|> unexpectedStringParser "Number must be 1"
  headList <- Item <$> (string ". " *> line)
  restList <- many (list n)
  return $ OrderedList (headList : restList)

orderedList :: Parser ADT
orderedList = isNotWhitespace *> orderedList' 0

------------------------------------------------------------

newline :: Parser ADT
newline = NewLine <$> is '\n'

modifiersArray :: [Parser ADT]
modifiersArray = [italic, bold, strikethrough, link, inlineCode, footnoteReference, footnote, image, code]

line :: Parser [ADT]
line = some (asum $ modifiersArray <|> [freeText])

markdownElement :: Parser [ADT]
markdownElement = some (asum (modifiersArray ++ [newline, heading, blockquote, orderedList, freeText]))

markdownParser :: Parser ADT
markdownParser = Paragraph <$> (inlineSpace *> markdownElement)

getTime :: IO String
getTime = formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%S" <$> getCurrentTime

-- convertADTHTML :: ADT -> String
-- convertADTHTML (Italic text) = "<i>" ++ text ++ "</i>"
-- convertADTHTML (Bold text) = "<b>" ++ text ++ "</b>"
-- convertADTHTML (Strikethrough text) = "<del>" ++ text ++ "</del>"
-- convertADTHTML (Link text url) = "<a href=\"" ++ url ++ "\">" ++ text ++ "</a>"
-- convertADTHTML (InlineCode code) = "<code>" ++ code ++ "</code>"
-- convertADTHTML (Footnote n) = "<sup>[" ++ show n ++ "]</sup>"
-- convertADTHTML (FreeText text) = text
-- convertADTHTML (Image alt src title) = "<img src=\"" ++ src ++ "\" alt=\"" ++ alt ++ "\" title=\"" ++ title ++ "\"/>"
-- convertADTHTML (Paragraph elements) = "<p>" ++ concatMap convertADTHTML elements ++ "</p>"
