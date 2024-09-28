module Assignment (markdownParser, convertADTHTML) where

import Control.Applicative (Alternative (..), Applicative (liftA2), asum, liftA3)
import Control.Monad (guard)
import Data.Char (isSpace)
import Data.Functor (($>))
import Data.List (dropWhileEnd, isPrefixOf)
import Data.Time.Clock (getCurrentTime)
import Data.Time.Format (defaultTimeLocale, formatTime)
import Instances (ParseError (..), ParseResult (..), Parser (..))
import Parser (anyChar, atLeast, between, betweenTwo, betweenTwoTok, charTok, checkHash, checkHeadingSep, eof, getContent, inlineSpace, is, isNot, isNotWhitespace, isOpeningTag, isPositiveInt, manyTill, noneof, oneofTok, optional, sepBy1, someTill, space, string, unexpectedStringParser, untilChar, untilIncludingCharTok)

data ADT
  = Italic String
  | Bold String
  | Strikethrough String
  | Link String String
  | InlineCode String
  | Footnote Int
  | Image String String String
  | FootnoteReference Int String
  | Text String
  | FreeText [ADT]
  | Heading Int [ADT]
  | Blockquote [ADT]
  | Code String String
  | Item [ADT]
  | OrderedList [ADT]
  | Row [ADT]
  | Header [ADT]
  | Body [[ADT]]
  | Table ADT ADT
  | NewLine Char
  | Markdown [ADT]
  deriving (Show, Eq)

-- $setup
-- >>> import Instances (isErrorResult, parse)

getResult :: ParseResult a -> (a -> String) -> String
getResult (Result _ a) f = f a
getResult _ _ = ""

italic :: Parser ADT
italic = Italic <$> between "_"

bold :: Parser ADT
bold = Bold <$> between "**"

strikethrough :: Parser ADT
strikethrough = Strikethrough <$> between "~~"

link :: Parser ADT
link = liftA2 Link (betweenTwoTok "[" "]") (betweenTwo "(" ")")

inlineCode :: Parser ADT
inlineCode = InlineCode <$> between "`"

footnote' :: Parser Int
footnote' = inlineSpace *> string "[^" *> isPositiveInt <* is ']'

footnote :: Parser ADT
footnote = Footnote <$> footnote'

-- can there be whitespace between ] and (?
-- what about whitespace between " and )?
-- >>> parse image "![Alt text](/path/to/img.jpg \"Optional title\")"
-- Result >< Image "Alt text" "/path/to/img.jpg" "Optional title"
image :: Parser ADT
image =
  liftA3
    Image
    (inlineSpace *> is '!' *> isNotWhitespace *> betweenTwoTok "[" "]")
    (betweenTwo "(" " \"")
    (untilIncludingCharTok '"' <* is ')')

-- >>> parse footnoteReference "[^1]: My reference.\n[^2]:Another reference.\n[^3]:  The 2 spaces after the colon should be ignored"
-- Unexpected character: ":"
footnoteReference :: Parser ADT
footnoteReference = liftA2 FootnoteReference footnote' (charTok ':' *> untilChar '\n')

tagsArr :: [String]
tagsArr = ["\n", "_", "**", "~~", "[", "`", "[^", "![", "|"]

text' :: [String] -> Parser Char
text' tags = Parser f
  where
    f "" = Error UnexpectedEof
    f input@(x : xs)
      | any (`isPrefixOf` input) tags = Error $ UnexpectedString "Text modifiers are not allowed in free text"
      | otherwise = Result xs x

text :: [String] -> Parser ADT
text tags = Text <$> some (text' tags)

freeText' :: Parser [ADT]
freeText' = some (asum $ modifiersArray ++ [text tagsArr])

-- >>> parse markdownParser "Here is some **markdown**\nMore lines here\nText"
-- Result >< Markdown [FreeText [Text "Here is some ",Bold "markdown"],NewLine '\n',FreeText [Text "More lines here"],NewLine '\n',FreeText [Text "Text"]]
freeText :: Parser ADT
freeText = FreeText <$> freeText'

heading :: Parser ADT
heading = liftA2 Heading checkHash (space *> freeText') <|> flip Heading <$> (freeText' <* is '\n') <*> checkHeadingSep

-- >>> parse blockquote "> This is a block quote.\n> It can **span** multiple lines."
-- Result >< Blockquote [FreeText [Text "This is a block quote."],FreeText [Text "It can ",Bold "span",Text " multiple lines."]]
blockquote :: Parser ADT
blockquote = Blockquote <$> some (inlineSpace *> charTok '>' *> freeText <* optional (is '\n'))

-- does the closing ``` have to be in a new line?
-- >>> parse code "```haskell\nblockquote\n``````\nblockquote\n```"
-- Result >< Code "haskell" "blockquote\n``````\nblockquote"
-- >>> parse code "```haskell\nmain :: IO ()\nmain = do\n    putStrLn \"Never gonna give you up\"\n    putStrLn \"Never gonna let you down\"\n    putStrLn \"Never gonna run around and desert you\"\n```\n"
-- Result >< Code "haskell" "main :: IO ()\nmain = do\n    putStrLn \"Never gonna give you up\"\n    putStrLn \"Never gonna let you down\"\n    putStrLn \"Never gonna run around and desert you\""
code :: Parser ADT
code = do
  language <- inlineSpace *> isOpeningTag "```" *> manyTill (isNot '\n') (is '\n')
  content <- someTill anyChar (string "\n```" <* (eof <|> is '\n' $> ()))
  return $ Code language content

-- code = liftA2 Code (inlineSpace *> isOpeningTag "```" *> manyTill (isNot '\n') (string "\n```")) (getContent "\n```" <* (eof <|> is '\n' $> ()))

nestedList :: Int -> Parser ADT
nestedList n = string (replicate (n + 4) ' ') *> orderedList' (n + 4)

-- need to check > 1?
listContent :: Int -> Parser ADT
listContent n = string (replicate n ' ') *> isPositiveInt *> string ". " *> (Item <$> freeText')

list :: Int -> Parser ADT
list = (is '\n' *>) . liftA2 (<|>) nestedList listContent

orderedList' :: Int -> Parser ADT
orderedList' n = do
  number <- isPositiveInt
  guard (number == 1) <|> unexpectedStringParser "Number must be 1"
  headList <- Item <$> (string ". " *> freeText')
  restList <- many (list n)
  return $ OrderedList (headList : restList)

-- >>> parse orderedList "1. Item 1\n    1. Sub Item 1\n    2. Sub Item 2\n        1. Sub Sub Item 1\n2. **Bolded Item 2**\n6. Item 3\n7. Item 4"
-- Result >< OrderedList [Item [Text "Item 1"],OrderedList [Item [Text "Sub Item 1"],Item [Text "Sub Item 2"],OrderedList [Item [Text "Sub Sub Item 1"]]],Item [Bold "Bolded Item 2"],Item [Text "Item 3"],Item [Text "Item 4"]]
orderedList :: Parser ADT
orderedList = isNotWhitespace *> orderedList' 0

-- >>> parse cell " h   er**e**          |"
-- Result >< " h   er**e**"
cell :: Parser ADT
cell = Text <$> someTill (noneof "\n|") (inlineSpace *> oneofTok "\n|")

-- row :: Parser [ADT]
-- row = Row <$> sepBy1 (some (asum (modifiersArray ++ [cell]))) (inlineSpace *> charTok '|')

checkNCol :: Int -> [a] -> Parser [a]
checkNCol nCol values = do
  guard (length values == nCol) <|> unexpectedStringParser "Number of columns in table header and row do not match"
  return values

checkTableSep :: Int -> Parser [String]
checkTableSep nCol = do
  sep <- sepBy1 (inlineSpace *> atLeast 3 (is '-')) (inlineSpace *> charTok '|')
  checkNCol nCol sep

-- >>> parse table "  | Tables        | Are           | Cool  |\n| ------------- | ------------- | ----- |\n| here          | is            | data  |\n| here          | is            | data  |\n| here | is also | **bolded data** |\n| also | part of the | table |"
-- Result >< Table (Header [Item [Text "Tables        "],Item [Text "Are           "],Item [Text "Cool  "]]) (Body [[Item [Text "here          "],Item [Text "is            "],Item [Text "data  "]],[Item [Text "here          "],Item [Text "is            "],Item [Text "data  "]],[Item [Text "here "],Item [Text "is also "],Item [Bold "bolded data",Text " "]],[Item [Text "also "],Item [Text "part of the "],Item [Text "table "]]])
table :: Parser ADT
table = do
  header <- row
  _ <- checkTableSep (length header)
  body <- Body <$> some (row >>= checkNCol (length header))
  return $ Table (Header header) body

------------------------------------------------------------

newline :: Parser ADT
newline = NewLine <$> is '\n'

modifiersArray :: [Parser ADT]
modifiersArray = [italic, bold, strikethrough, link, inlineCode, footnote]

markdownElement :: Parser [ADT]
markdownElement = some (asum ([newline, image, footnoteReference, heading, blockquote, code, orderedList, table] ++ modifiersArray ++ [freeText]))

markdownParser :: Parser ADT
markdownParser = Markdown <$> (inlineSpace *> markdownElement)

getTime :: IO String
getTime = formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%S" <$> getCurrentTime

tag :: String -> String -> String
tag t content = "<" ++ t ++ ">" ++ content ++ "</" ++ t ++ ">"

block :: String -> String -> String
block t content = indent 4 ("<" ++ t ++ ">") ++ indent 4 content ++ indent 4 ("</" ++ t ++ ">")

-- Helper to indent content by `n` spaces and add a newline at the end
indent :: Int -> String -> String
indent n = unlines . map (replicate n ' ' ++) . lines

convertADTHTML :: ADT -> String
convertADTHTML (Markdown adt) =
  "<!DOCTYPE html>\n<html lang=\"en\">\n\n<head>\n    <meta charset=\"UTF-8\">\n    <title>Test</title>\n</head>\n\n<body>\n"
    ++ concatMap convertADTHTML adt
    ++ "</body>\n\n</html>\n"
convertADTHTML (NewLine _) = ""
convertADTHTML (Italic text) = tag "em" text
convertADTHTML (Bold text) = tag "strong" text
convertADTHTML (Strikethrough text) = tag "del" text
convertADTHTML (Link text url) = "<a href=\"" ++ url ++ "\">" ++ text ++ "</a>\n"
convertADTHTML (InlineCode code) = tag "code" code
convertADTHTML (Footnote n) = "<sup><a id=\"fn" ++ show n ++ "ref\" href=\"#fn" ++ show n ++ "\">" ++ show n ++ "</a></sup>\n"
convertADTHTML (Image alt src title) = indent 4 (tag "p" ("<img src=\"" ++ src ++ "\" alt=\"" ++ alt ++ "\" title=\"" ++ title ++ "\">"))
convertADTHTML (FootnoteReference n reference) = indent 4 $ "<p id=\"fn" ++ show n ++ "\">" ++ reference ++ "</p>"
convertADTHTML (Text text) = text
convertADTHTML (FreeText adt) = indent 4 (tag "p" (concatMap convertADTHTML adt))
convertADTHTML (Heading n adt) = indent 4 (tag ("h" ++ show n) (concatMap convertADTHTML adt))
convertADTHTML (Blockquote adt) = block "blockquote" (concatMap convertADTHTML adt)
convertADTHTML (Code language code) = replicate 4 ' ' ++ tag "pre" ("<code class=\"language-" ++ language ++ "\">" ++ code ++ "</code>") ++ "\n"
convertADTHTML (Item adt) = indent 4 (tag "li" (concatMap convertADTHTML adt))
convertADTHTML (OrderedList adt) = block "ol" (concatMap convertADTHTML adt)
convertADTHTML (Header adt) = block "thead" (block "tr" (indent 4 (tag "th" (concatMap convertADTHTML adt))))
convertADTHTML (Body adt) = block "tbody" (concatMap (block "tr" . concatMap convertADTHTML) adt)
convertADTHTML (Table header body) = block "table" (convertADTHTML header ++ convertADTHTML body)
