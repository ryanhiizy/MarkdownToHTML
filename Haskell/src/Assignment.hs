module Assignment (markdownParser, convertADTHTML, getTime, convertADTHTMLwithTitle) where

import Control.Applicative (Alternative (..), Applicative (liftA2), liftA3,
                            optional)
import Control.Monad (guard)
import Data.Foldable (asum)
import Data.Time.Clock (getCurrentTime)
import Data.Time.Format (defaultTimeLocale, formatTime)
import Instances (ParseError (..), ParseResult (..), Parser (..))
import Parser (atLeast, charTok, inlineSpace, is, isEnd, isNot, isNotWhitespace,
              positiveInt, lookAhead, manyCharTill, sepBy1, someCharTill, space,
              spaces, string, unexpectedStringParser, someTill)
import Data.Maybe (fromMaybe)
import Data.List (isPrefixOf)

data ADT
  = Italic [ADT]
  | Bold [ADT]
  | Strikethrough [ADT]
  | Link [ADT] String
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
  | Newline Int
  | Markdown [ADT]
  deriving (Show, Eq)

-- $setup
-- >>> import Instances (isErrorResult, parse)


---- Text Modifiers ----

italic :: Parser ADT
italic = Italic <$> nestedBetween "_"

bold :: Parser ADT
bold = Bold <$> nestedBetween "**"

strikethrough :: Parser ADT
strikethrough = Strikethrough <$> nestedBetween "~~"

link :: Parser ADT
link = liftA2
        Link
        (nestedbetweenTwo "[" "]")
        (inlineSpace *> betweenTwo "(" ")")

inlineCode :: Parser ADT
inlineCode = InlineCode <$> between "`"

footnote :: Parser ADT
footnote = Footnote <$> footnote'


---- Text Modifier Helpers ----

footnote' :: Parser Int
footnote' = string "[^" *> positiveInt <* is ']'

between :: String -> Parser String
between = liftA2 (*>) openingTag (someCharTill . string)

betweenTwo :: String -> String -> Parser String
betweenTwo opening closing = openingTag opening *> someCharTill (string closing)

openingTag :: String -> Parser String
openingTag t = do
  result <- string t
  _ <- lookAhead (isNot (head t)) -- Prevent parsing cases such as "***a**"
  return result

modifiers :: [Parser ADT]
modifiers = [italic, bold, strikethrough, link, inlineCode, footnote]

nested :: String -> Parser ADT
nested t = Text <$> some (Parser f)
  where
    f "" = Error UnexpectedEof
    f (x : _) | x == '\n' = Error $ UnexpectedString "Newline found"
    f input@(x : xs) = case parse (string t) input of
      Result _ _ -> Error $ UnexpectedString "Closing tag found"
      Error _ -> Result xs x

nestedBetween :: String -> Parser [ADT]
nestedBetween t = do
  _ <- string t
  _ <- lookAhead (isNot (head t)) -- Prevent parsing cases such as "***a**"
  someTill (asum modifiers <|> nested t) (string t)

nestedbetweenTwo :: String -> String -> Parser [ADT]
nestedbetweenTwo opening closing = do
  _ <- string opening
  _ <- lookAhead (isNot (head opening)) -- Prevent parsing cases such as "***a**"
  someTill (asum modifiers <|> nested closing) (string closing)


---- Image ----

image :: Parser ADT
image = liftA3
          Image
          (inlineSpace *> is '!' *> isNotWhitespace *> betweenTwo "[" "]")
          (betweenTwo "(" " \"")
          (someCharTill (string "\")"))


--- Footnote Reference ---

footnoteReference :: Parser ADT
footnoteReference = liftA2
                      FootnoteReference (inlineSpace *> footnote')
                      (charTok ':' *> some (isNot '\n'))


---- Free Text ----

freeText :: Parser ADT
freeText = FreeText <$> freeText'


---- Free Text Helpers ----

tagsArr :: [String]
tagsArr = ["\n", "_", "**", "~~", "[", "`", "[^", "![", "|"]

text' :: [String] -> Parser Char
text' tags = Parser f
  where
    f "" = Error UnexpectedEof
    f input@(x : xs)
      | any (`isPrefixOf` input) tags = Error $ UnexpectedString "Text modifiers found"
      | otherwise = Result xs x

text :: [String] -> Parser ADT
text tags = Text <$> some (text' tags)

freeText' :: Parser [ADT]
freeText' = some (asum modifiers <|> text tagsArr <|>
             (Text . (:[]) <$> isNot '\n'))


---- Heading ----

heading :: Parser ADT
heading = liftA2 Heading checkHash (space *> freeText')
          <|>
          liftA2 (flip Heading) (freeText' <* charTok '\n') checkHeadingSep


---- Heading Helpers ----

checkHash :: Parser Int
checkHash = do
  hashes <- inlineSpace *> some (is '#')
  guard (length hashes <= 6) <|> unexpectedStringParser "Too many hashes"
  return $ length hashes

checkHeadingSep :: Parser Int
checkHeadingSep = do
  seps <- atLeast 2 '=' <|> atLeast 2 '-'
  _ <- inlineSpace *> isEnd
  return $ if head seps == '=' then 1 else 2


---- Blockquote ----

blockquote :: Parser ADT
blockquote = Blockquote
              <$> some (optional (is '\n')
              *> inlineSpace *> charTok '>' *> freeText)


---- Code ----

code :: Parser ADT
code = do
  language <- inlineSpace *> openingTag "```" *> manyCharTill (is '\n')
  body <- someCharTill (string "\n```" <* isEnd)
  return $ Code language body


---- Ordered List ----

orderedList :: Parser ADT
orderedList = isNotWhitespace *> orderedList' 0


--- Ordered List Helpers ---

nestedList :: Int -> Parser ADT
nestedList n = string (replicate (n + 4) ' ') *> orderedList' (n + 4)

listContent :: Int -> Parser ADT
listContent n = string (replicate n ' ')
                *> positiveInt
                *> string ". "
                *> (Item <$> freeText')

list :: Int -> Parser ADT
list = (is '\n' *>) . liftA2 (<|>) nestedList listContent

orderedList' :: Int -> Parser ADT
orderedList' n = do
  number <- positiveInt
  guard (number == 1) <|> unexpectedStringParser "Number must be 1"
  headList <- Item <$> (string ". " *> freeText')
  restList <- many (list n)
  return $ OrderedList (headList : restList)


---- Table ----

table :: Parser ADT
table = do
  header <- inlineSpace *> row
  _ <- checkTableSep (length header)
  body <- Body <$> some (row >>= checkNCol (length header))
  return $ Table (Header header) body


---- Table Helpers ----

cell :: Parser ADT
cell = Text
        <$> some (lookAhead (inlineSpace *> isNot '|') *> is ' '
        <|> text' (tagsArr ++ [" "]))

row :: Parser [[ADT]]
row = sepBy1
        (inlineSpace *> some (asum modifiers <|> cell))
        (optional (is '\n') *> inlineSpace *> is '|')

checkNCol :: Int -> [a] -> Parser [a]
checkNCol nCol values = do
  guard (length values == nCol) <|> unexpectedStringParser "Number of columns in table header and row do not match"
  return values

checkTableSep :: Int -> Parser [String]
checkTableSep nCol = do
  sep <- sepBy1
          (inlineSpace *> atLeast 3 '-')
          (optional (is '\n') *> inlineSpace *> is '|')
  checkNCol nCol sep


---- Conversion ----

reverseInput :: Parser String
reverseInput = Parser $ \input -> Result (reverse input) ""

trim :: Parser String
trim = spaces *> reverseInput <* spaces *> reverseInput

newline :: Parser ADT
newline = do
  n <- some (is '\n')
  return $ Newline (length n)

markdown :: [Parser ADT]
markdown = [newline, image, footnoteReference, heading,
            blockquote, code, orderedList, table, freeText]

markdownElement :: Parser [ADT]
markdownElement = some (asum markdown)

markdownParser :: Parser ADT
markdownParser = Markdown <$> (trim *> markdownElement)

getTime :: IO String
getTime = formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%S" <$> getCurrentTime

indent :: Int -> String -> String
indent n = unlines . map (replicate n ' ' ++) . lines

tag :: String -> String -> String
tag t c = "<" ++ t ++ ">" ++ c ++ "</" ++ t ++ ">"

block :: String -> String -> String
block t c = indent 4 ("<" ++ t ++ ">") ++ indent 4 c
            ++ indent 4 ("</" ++ t ++ ">")

convertADTHTMLwithTitle :: String -> ADT -> String
convertADTHTMLwithTitle title (Markdown adt) = convertMarkdown (Just title) adt
convertADTHTMLwithTitle _ _ = "Invalid Input"

convertADTHTML :: ADT -> String
convertADTHTML (Markdown adt) = convertMarkdown Nothing adt
convertADTHTML (Newline n) = convertNewline n
convertADTHTML (Italic i) = convertItalic i
convertADTHTML (Bold b) = convertBold b
convertADTHTML (Strikethrough s) = convertStrikethrough s
convertADTHTML (Link l url) = convertLink l url
convertADTHTML (InlineCode inline) = convertInlineCode inline
convertADTHTML (Footnote n) = convertFootnote n
convertADTHTML (Image alt src title) = convertImage alt src title
convertADTHTML (FootnoteReference n ref) = convertFootnoteReference n ref
convertADTHTML (Text t) = t
convertADTHTML (FreeText adt) = convertFreeText adt
convertADTHTML (Heading n adt) = convertHeading n adt
convertADTHTML (Blockquote adt) = convertBlockquote adt
convertADTHTML (Code language c) = convertCode language c
convertADTHTML (Item adt) = convertItem adt
convertADTHTML (OrderedList adt) = convertOrderedList adt
convertADTHTML (Header adt) = convertHeader adt
convertADTHTML (Body adt) = convertBody adt
convertADTHTML (Table header body) = convertTable header body

convertMarkdown :: Maybe String -> [ADT] -> String
convertMarkdown maybeTitle adt =
  let title = fromMaybe "Test" maybeTitle
  in "<!DOCTYPE html>\n<html lang=\"en\">\n\n<head>\n    <meta charset=\"UTF-8\">\n    <title>"
     ++ title
     ++ "</title>\n</head>\n\n<body>\n"
     ++ concatMap convertADTHTML adt
     ++ "</body>\n\n</html>\n"

convertNewline :: Int -> String
convertNewline n
  | n == 1 = ""
  | otherwise = indent 4 (tag "p" "")

convertItalic :: [ADT] -> String
convertItalic = tag "em" . concatMap convertADTHTML

convertBold :: [ADT] -> String
convertBold = tag "strong" . concatMap convertADTHTML

convertStrikethrough :: [ADT] -> String
convertStrikethrough = tag "del" . concatMap convertADTHTML

convertLink :: [ADT] -> String -> String
convertLink adt url = "<a href=\"" ++ url ++ "\">"
                      ++ concatMap convertADTHTML adt ++ "</a>"

convertInlineCode :: String -> String
convertInlineCode = tag "code"

convertFootnote :: Int -> String
convertFootnote n = tag "sup" ("<a id=\"fn" ++ show n ++ "ref\" href=\"#fn"
                    ++ show n ++ "\">" ++ show n ++ "</a>")

convertImage :: String -> String -> String -> String
convertImage alt src title = indent 4 (tag "p" ("<img src=\"" ++ src
                              ++ "\" alt=\"" ++ alt ++ "\" title=\""
                              ++ title ++ "\">"))

convertFootnoteReference :: Int -> String -> String
convertFootnoteReference n reference = indent 4 ("<p id=\"fn" ++ show n
                                        ++ "\">" ++ reference ++ "</p>")

convertFreeText :: [ADT] -> String
convertFreeText adt = indent 4 (tag "p" (concatMap convertADTHTML adt))

convertHeading :: Int -> [ADT] -> String
convertHeading n adt = indent 4
                        (tag ("h" ++ show n)
                        (concatMap convertADTHTML adt))

convertBlockquote :: [ADT] -> String
convertBlockquote adt = block "blockquote" (concatMap convertADTHTML adt)

convertCode :: String -> String -> String
convertCode "" c = replicate 4 ' '
                    ++ tag "pre" ("<code>" ++ c ++ "</code>") ++ "\n"
convertCode language c = replicate 4 ' ' ++ tag "pre" ("<code class=\"language-"
                          ++ language ++ "\">" ++ c ++ "</code>") ++ "\n"

convertItem :: [ADT] -> String
convertItem adt = indent 4 (tag "li" (concatMap convertADTHTML adt))

convertOrderedList :: [ADT] -> String
convertOrderedList adt = block "ol" (concatMap convertADTHTML adt)

convertTable :: ADT -> ADT -> String
convertTable headerRows bodyRows = block "table" (convertADTHTML headerRows
                                    ++ convertADTHTML bodyRows)

convertHeader :: [[ADT]] -> String
convertHeader = convertRow "th"

convertBody :: [[[ADT]]] -> String
convertBody = concatMap (convertRow "td")

convertRow :: String -> [[ADT]] -> String
convertRow t r = block "tr" (concatMap (convertCell t) r)

convertCell :: String -> [ADT] -> String
convertCell t c = indent 4 (tag t (concatMap convertADTHTML c))
