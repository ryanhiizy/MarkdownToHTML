module Assignment (markdownParser, convertADTHTML) where

import Control.Applicative (Alternative (..), Applicative (liftA2), asum, liftA3)
import Control.Monad (guard)
import Data.List (isPrefixOf)
import Data.Time.Clock (getCurrentTime)
import Data.Time.Format (defaultTimeLocale, formatTime)
import Instances (ParseError (..), ParseResult (..), Parser (..))
import Parser (anyChar, atLeast, between, betweenTwo, betweenTwoTok, charTok, checkHash, checkHeadingSep, inlineSpace, is, isEnd, isNot, isNotWhitespace, isOpeningTag, isPositiveInt, lookAhead, manyTill, optional, sepBy1, someCharTill, space, spaces, string, unexpectedStringParser)

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
  | Header [[ADT]]
  | Body [[[ADT]]]
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
    (someCharTill (charTok '"') <* is ')')

-- >>> parse footnoteReference "[^1]: My reference.\n[^2]:Another reference.\n[^3]:  The 2 spaces after the colon should be ignored"
-- >>> parse footnoteReference "[^1]: My reference."
-- Result >
-- [^2]:Another reference.
-- [^3]:  The 2 spaces after the colon should be ignored< FootnoteReference 1 "My reference."
-- Result >< FootnoteReference 1 "My reference."
footnoteReference :: Parser ADT
footnoteReference = liftA2 FootnoteReference footnote' (charTok ':' *> some (isNot '\n'))

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
heading = liftA2 Heading checkHash (space *> freeText') <|> flip Heading <$> (freeText' <* charTok '\n') <*> checkHeadingSep

-- >>> parse blockquote "> This is a block quote.\n> It can **span** multiple lines."
-- Result >< Blockquote [FreeText [Text "This is a block quote."],FreeText [Text "It can ",Bold "span",Text " multiple lines."]]
blockquote :: Parser ADT
blockquote = Blockquote <$> some (inlineSpace *> charTok '>' *> freeText <* optional (is '\n'))

-- does the closing ``` have to be in a new line?
-- >>> parse code "```haskell\nblockquote\n``````\nblockquote\n```\n"
-- >>> parse code "```haskell\nmain :: IO ()\nmain = do\n    putStrLn \"Never gonna give you up\"\n    putStrLn \"Never gonna let you down\"\n    putStrLn \"Never gonna run around and desert you\"\n```\n"
-- Result >
-- < Code "haskell" "blockquote\n``````\nblockquote"
-- Result >
-- < Code "haskell" "main :: IO ()\nmain = do\n    putStrLn \"Never gonna give you up\"\n    putStrLn \"Never gonna let you down\"\n    putStrLn \"Never gonna run around and desert you\""
code :: Parser ADT
code = do
  language <- inlineSpace *> isOpeningTag "```" *> manyTill anyChar (is '\n')
  content <- someCharTill (string "\n```" <* isEnd)
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

-- >>> parse cell "a "
-- Result > < Text "a"
cell :: Parser ADT
cell = Text <$> some (lookAhead (inlineSpace *> isNot '|') *> is ' ' <|> text' (tagsArr ++ ["|", " "]))

-- >>> parse row "|aasd ryan |"
-- Result >< [[Text "aasd ryan"]]
row :: Parser [[ADT]]
row = sepBy1 (some (asum (modifiersArray ++ [cell]))) (inlineSpace *> charTok '|')

checkNCol :: Int -> [a] -> Parser [a]
checkNCol nCol values = do
  guard (length values == nCol) <|> unexpectedStringParser "Number of columns in table header and row do not match"
  return values

checkTableSep :: Int -> Parser [String]
checkTableSep nCol = do
  sep <- sepBy1 (inlineSpace *> atLeast 3 (is '-')) (inlineSpace *> charTok '|')
  checkNCol nCol sep

-- >>> parse table "  | Tables    **bold**    |Are| Cool  |\n| ------------- | ------------- | ----- |\n| here          | is            | data  |\n| here          | is            | data  |\n| here | is also | **bolded data** |\n| also | part of the | table |"
-- Result >< Table (Header [[Text "Tables    ",Bold "bold"],[Text "Are"],[Text "Cool"]]) (Body [[[Text "here"],[Text "is"],[Text "data"]],[[Text "here"],[Text "is"],[Text "data"]],[[Text "here"],[Text "is also"],[Bold "bolded data"]],[[Text "also"],[Text "part of the"],[Text "table"]]])
table :: Parser ADT
table = do
  header <- inlineSpace *> row
  _ <- checkTableSep (length header)
  body <- Body <$> some (row >>= checkNCol (length header))
  return $ Table (Header header) body

------------------------------------------------------------

newline :: Parser ADT
newline = NewLine <$> charTok '\n'

modifiersArray :: [Parser ADT]
modifiersArray = [italic, bold, strikethrough, link, inlineCode, footnote]

markdownElement :: Parser [ADT]
markdownElement = some (asum ([newline, image, footnoteReference, heading, blockquote, code, orderedList, table] ++ modifiersArray ++ [freeText]))

-- >>> parse reverseInput "abc  "
-- Result >  cba< ""
reverseInput :: Parser String
reverseInput = Parser $ \input -> Result (reverse input) ""

-- >>> parse trimTrailingWhitespace "abcs sdsd \n \t "
-- Result >abcs sdsd< ""
trimTrailingWhitespace :: Parser String
trimTrailingWhitespace = reverseInput <* spaces *> reverseInput

markdownParser :: Parser ADT
markdownParser = Markdown <$> (spaces *> trimTrailingWhitespace *> markdownElement)

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
convertADTHTML (Markdown adt) = convertMarkdown adt
convertADTHTML (NewLine _) = ""
convertADTHTML (Italic i) = convertItalic i
convertADTHTML (Bold b) = convertBold b
convertADTHTML (Strikethrough s) = convertStrikethrough s
convertADTHTML (Link l url) = convertLink l url
convertADTHTML (InlineCode inline) = convertInlineCode inline
convertADTHTML (Footnote n) = convertFootnote n
convertADTHTML (Image alt src title) = convertImage alt src title
convertADTHTML (FootnoteReference n reference) = convertFootnoteReference n reference
convertADTHTML (Text t) = convertText t
convertADTHTML (FreeText adt) = convertFreeText adt
convertADTHTML (Heading n adt) = convertHeading n adt
convertADTHTML (Blockquote adt) = convertBlockquote adt
convertADTHTML (Code language c) = convertCode language c
convertADTHTML (Item adt) = convertItem adt
convertADTHTML (OrderedList adt) = convertOrderedList adt
convertADTHTML (Header adt) = convertHeader adt
convertADTHTML (Body adt) = convertBody adt
convertADTHTML (Table header body) = convertTable header body

convertMarkdown :: [ADT] -> String
convertMarkdown adt =
  "<!DOCTYPE html>\n<html lang=\"en\">\n\n<head>\n    <meta charset=\"UTF-8\">\n    <title>Test</title>\n</head>\n\n<body>\n"
    ++ concatMap convertADTHTML adt
    ++ "</body>\n\n</html>\n"

convertNewLine :: ADT -> String
convertNewLine _ = ""

convertItalic :: String -> String
convertItalic = tag "em"

convertBold :: String -> String
convertBold = tag "strong"

convertStrikethrough :: String -> String
convertStrikethrough = tag "del"

convertLink :: String -> String -> String
convertLink t url = "<a href=\"" ++ url ++ "\">" ++ t ++ "</a>\n"

convertInlineCode :: String -> String
convertInlineCode = tag "code"

convertFootnote :: Int -> String
convertFootnote n = "<sup><a id=\"fn" ++ show n ++ "ref\" href=\"#fn" ++ show n ++ "\">" ++ show n ++ "</a></sup>\n"

convertImage :: String -> String -> String -> String
convertImage alt src title = indent 4 (tag "p" ("<img src=\"" ++ src ++ "\" alt=\"" ++ alt ++ "\" title=\"" ++ title ++ "\">"))

convertFootnoteReference :: Int -> String -> String
convertFootnoteReference n reference = indent 4 ("<p id=\"fn" ++ show n ++ "\">" ++ reference ++ "</p>")

convertText :: String -> String
convertText t = t

convertFreeText :: [ADT] -> String
convertFreeText adt = indent 4 (tag "p" (concatMap convertADTHTML adt))

convertHeading :: Int -> [ADT] -> String
convertHeading n adt = indent 4 (tag ("h" ++ show n) (concatMap convertADTHTML adt))

convertBlockquote :: [ADT] -> String
convertBlockquote adt = block "blockquote" (concatMap convertADTHTML adt)

convertCode :: String -> String -> String
convertCode language c = replicate 4 ' ' ++ tag "pre" ("<code class=\"language-" ++ language ++ "\">" ++ c ++ "</code>") ++ "\n"

convertItem :: [ADT] -> String
convertItem adt = indent 4 (tag "li" (concatMap convertADTHTML adt))

convertOrderedList :: [ADT] -> String
convertOrderedList adt = block "ol" (concatMap convertADTHTML adt)

-- Convert the entire table (header and body)
convertTable :: ADT -> ADT -> String
convertTable headerRows bodyRows = block "table" (convertADTHTML headerRows ++ convertADTHTML bodyRows)

convertHeader :: [[ADT]] -> String
convertHeader = convertRow "th"

convertBody :: [[[ADT]]] -> String
convertBody = concatMap (convertRow "td")

-- Convert a single row of ADT cells into a <tr> with <td> tags
convertRow :: String -> [[ADT]] -> String
convertRow t r = block "tr" (concatMap (convertCell t) r)

-- Convert a single cell (list of ADTs) into a <td> tag
convertCell :: String -> [ADT] -> String
convertCell t c = indent 4 (tag t (concatMap convertADTHTML c))
