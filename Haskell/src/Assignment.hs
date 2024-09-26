module Assignment (markdownParser) where

import Control.Applicative (Alternative (..), Applicative (liftA2), asum, liftA3, optional)
import Control.Monad (guard)
import Data.Time.Clock (getCurrentTime)
import Data.Time.Format (defaultTimeLocale, formatTime)
import Instances (ParseError (..), ParseResult (..), Parser (..))
import Parser (between, betweenCode, charTok, getContent, hashLength, headingSepLength, inlineSpace, is, isNotWhitespace, isPositiveInt, space, string, untilChar, untilIncludingChar, untilIncludingCharTok)

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
  | OrderedList Int Int [ADT]
  | NewLine Char
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
    (is '(' *> getContent " \"" <* string " \"")
    (untilIncludingCharTok '"' <* is ')')

footnoteReference :: Parser ADT
footnoteReference = liftA2 FootnoteReference footnote (charTok ':' *> untilChar '\n')

freeText' :: Parser Char
freeText' = Parser f
  where
    f "" = Error UnexpectedEof
    f (x : _) | x == '\n' = Error $ UnexpectedString "Newline found"
    f input@(x : xs) = case parse (some $ asum modifiersArray) input of
      Result _ _ -> Error $ UnexpectedString "Text modifiers are not allowed in free text"
      Error _ -> Result xs x

freeText :: Parser ADT
freeText = FreeText <$> some freeText'

heading :: Parser ADT
heading = liftA2 Heading hashLength (space *> line) <|> flip Heading <$> (line <* is '\n') <*> headingSepLength

blockquote :: Parser ADT
blockquote = Blockquote <$> (inlineSpace *> charTok '>' *> line)

code :: Parser ADT
code = Code <$> betweenCode "```"

validateOrder :: Bool -> Parser Int
validateOrder first = do
  n <- isPositiveInt
  guard (if first then n == 1 else n > 1)
  return n

orderedList' :: Bool -> Int -> Parser [ADT]
orderedList' first indentLevel = do
  n <- validateOrder first
  _ <- string ". "
  line' <- line
  _ <- optional (is '\n')
  newIndentLevel <- length <$> many (is ' ')
  rest <-
    (guard (newIndentLevel == indentLevel) *> orderedList' False indentLevel)
      <|> (guard (newIndentLevel `mod` 4 == 0 && newIndentLevel <= indentLevel) *> orderedList' False newIndentLevel)
      <|> (guard (newIndentLevel == indentLevel + 4) *> orderedList' True newIndentLevel)
      <|> return []
  return $ OrderedList n indentLevel line' : rest

orderedList :: Parser ADT
orderedList = OrderedList 0 0 <$> (isNotWhitespace *> orderedList' True 0)

------------------------------------------------------------

newline :: Parser ADT
newline = NewLine <$> is '\n'

modifiersArray :: [Parser ADT]
modifiersArray = [italic, bold, strikethrough, link, inlineCode, footnote, image, footnoteReference, code]

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
