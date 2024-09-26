module Assignment (markdownParser) where

import Control.Applicative (Alternative (..), Applicative (liftA2), asum, liftA3)
import Data.Time.Clock (getCurrentTime)
import Data.Time.Format (defaultTimeLocale, formatTime)
import Instances (ParseError (..), ParseResult (..), Parser (..))
import Parser (anyChar, between, charTok, hashLength, headingSepLength, is, isNot, noLeadingSpaces, positiveInt, space, string, stringTok, without)

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
  | Line [ADT]
  | Paragraph [ADT]
  deriving (Show, Eq)

italic :: Parser ADT
italic = Italic <$> between "_"

bold :: Parser ADT
bold = Bold <$> between "**"

strikethrough :: Parser ADT
strikethrough = Strikethrough <$> between "~~"

link :: Parser ADT
link = liftA2 Link ((is '[' *> some (isNot ']')) <* stringTok "]") (is '(' *> some (isNot ')') <* is ')')

inlineCode :: Parser ADT
inlineCode = InlineCode <$> between "`"

footnote :: Parser ADT
footnote = Footnote <$> (string "[^" *> noLeadingSpaces *> positiveInt <* is ']')

image :: Parser ADT
image =
  liftA3
    Image
    (string "![" *> some (isNot ']') <* stringTok "]")
    (is '(' *> some (without " \"") <* string " \"")
    (some (isNot '"') <* string "\")")

footnoteReference :: Parser ADT
footnoteReference = liftA2 FootnoteReference footnote (charTok ':' *> some anyChar)

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
blockquote = Blockquote <$> (charTok '>' *> line)

modifiersArray :: [Parser ADT]
modifiersArray = [italic, bold, strikethrough, link, inlineCode, footnote, image, footnoteReference]

line :: Parser [ADT]
line = some (asum $ modifiersArray <|> [freeText])

markdownParser :: Parser ADT
markdownParser = Paragraph <$> (fmap (: []) heading <|> fmap (: []) blockquote <|> line)

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
