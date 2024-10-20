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
              spaces, string, unexpectedStringParser, someTill, oneof)
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

openingTag :: String -> Parser String
openingTag t = do
  -- Parse opening tag
  result <- string t
  -- Make sure there is no opening tag left to prevent parsing cases such as "***a**"
  _ <- lookAhead (isNot (head t))
  return result

-- Consumes opening tag and then parses any character until closing tag
between :: String -> Parser String
between = liftA2 (*>) openingTag (someCharTill . string)

-- Similar to `between` but for different opening and closing tags
betweenTwo :: String -> String -> Parser String
betweenTwo opening closing = openingTag opening *> someCharTill (string closing)

modifiers :: [Parser ADT]
modifiers = [italic, bold, strikethrough, link, inlineCode, footnote]

nested :: String -> Parser ADT
nested t = Text <$> some (Parser f)
  where
    f "" = Error UnexpectedEof
    f (x : _) | x == '\n' = Error $ UnexpectedString "Newline found"
    -- Try to parse closing tag, if successful, return Error, otherwise return parsed character
    f input@(x : xs) = case parse (string t) input of
      Result _ _ -> Error $ UnexpectedString "Closing tag found"
      Error _ -> Result xs x

nestedBetween :: String -> Parser [ADT]
nestedBetween t = do
  -- Parse opening tag
  _ <- string t
  -- Make sure there is no opening tag left to prevent parsing cases such as "***a**"
  _ <- lookAhead (isNot (head t))
  -- Repeatedly try to parse all the text modifiers,
  -- if none are found, parse current character using `nested`,
  -- then repeat the process until closing tag is found
  someTill (asum modifiers <|> nested t) (string t)

-- Similar to `nestedBetween` but for different opening and closing tags
nestedbetweenTwo :: String -> String -> Parser [ADT]
nestedbetweenTwo opening closing = do
  _ <- string opening
  _ <- lookAhead (isNot (head opening))
  someTill (asum modifiers <|> nested closing) (string closing)


---- Image ----

image :: Parser ADT
image = liftA3
          Image
          (inlineSpace *> is '!' *> isNotWhitespace *> betweenTwo "[" "]")
          -- Ensure there is a whitespace by including it in the closing tag
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
    -- Check if any of the tags are the prefix of the input
    f input@(x : xs)
      | any (`isPrefixOf` input) tags = Error $ UnexpectedString "Text modifiers found"
      | otherwise = Result xs x

text :: [String] -> Parser ADT
text tags = Text <$> some (text' tags)

-- Try all the text modifiers first, if none are found, parse characters
-- until either a possible tag or newline is found.
-- If neither of the above work, consume the current character and repeat the process.
-- This last case is used to parse characters from tags which are not text modifiers.
freeText' :: Parser [ADT]
freeText' = some (asum modifiers <|> text tagsArr <|>
             (Text . (:[]) <$> isNot '\n')) -- Converts parsed character to string


---- Heading ----

heading :: Parser ADT
heading = liftA2 Heading checkHash (oneof "\t\r\f\v " *> freeText')
          <|>
          -- For separator headings, the text is parsed before the separator,
          -- but the header level needs to be stored first, so flip is used.
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
  _ <- inlineSpace *> isEnd -- Ensure there is no text after the separator
  return $ if head seps == '=' then 1 else 2


---- Blockquote ----

blockquote :: Parser ADT
blockquote = Blockquote
              <$> some (optional (is '\n') *> -- For multiline blockquotes
                inlineSpace *> charTok '>' *> freeText)


---- Code ----

code :: Parser ADT
code = do
  -- manyCharTill is used to parse the optional language name
  language <- inlineSpace *> openingTag "```" *> manyCharTill (is '\n')
  -- Ensure the closing tag is on its own line
  body <- someCharTill (string "\n```" <* isEnd)
  return $ Code language body


---- Ordered List ----

orderedList :: Parser ADT
orderedList = isNotWhitespace *> orderedList' 0


--- Ordered List Helpers ---

-- Increase the indentation level then call the orderedList' function
-- since it's a new (sub)list
nestedList :: Int -> Parser ADT
nestedList n = string (replicate (n + 4) ' ') *> orderedList' (n + 4)

-- Parse the list item number and text with the same indentation level
listItem :: Int -> Parser ADT
listItem n = string (replicate n ' ')
                *> positiveInt
                *> string ". "
                *> (Item <$> freeText')

-- Try parsing a nested list first, if none are found, parse the next list item
list :: Int -> Parser ADT
list = (is '\n' *>) . liftA2 (<|>) nestedList listItem

-- Called every time a new list is started
orderedList' :: Int -> Parser ADT
orderedList' n = do
  -- Parse the first list item
  number <- positiveInt
  guard (number == 1) <|> unexpectedStringParser "Number must be 1"
  headList <- Item <$> (string ". " *> freeText')
  -- Recursively parse the rest of the list items
  restList <- many (list n)
  return $ OrderedList (headList : restList)


---- Table ----

table :: Parser ADT
table = do
  -- Parse the header row first
  header <- inlineSpace *> row
  -- Check if the separator row is valid
  _ <- checkTableSep (length header)
  -- Parse the body of the table while ensuring the number of columns match the header
  body <- Body <$> some (row >>= checkNCol (length header))
  return $ Table (Header header) body


---- Table Helpers ----

-- >>> parse cell "c |"
-- Result > |< Text "c"
cell :: Parser ADT
cell = Text
        -- Break instantly if the current character is not a whitespace, (breaks with inlineSpace)
        -- if it is, check if there is still text after the whitespace, (breaks with isNot '|')
        -- if there isn't, parse the current whitespace character, (parses with oneof "\t\r\f\v ")
        -- else, both this and text' will fail, effectively trimming trailing whitespaces.
        <$> some (lookAhead (inlineSpace *> isNot '|') *> oneof "\t\r\f\v " <|>
          -- Parse characters until a possible tag or whitespace is found
          text' (tagsArr ++ ["\t", "\r", "\f", "\v", " "]))

row :: Parser [[ADT]]
row = sepBy1
        -- Cell parser
        -- Try to parse all the text modifiers first, if none are found,
        -- parse characters using `cell`.
        (inlineSpace *> some (asum modifiers <|> cell))
        -- Separator parser
        (optional (is '\n') *> inlineSpace *> is '|')

-- Check if the number of columns in the header and row match
checkNCol :: Int -> [a] -> Parser [a]
checkNCol nCol values = do
  guard (length values == nCol) <|> unexpectedStringParser "Number of columns in table header and row do not match"
  return values

checkTableSep :: Int -> Parser [String]
checkTableSep nCol = do
  -- Parse the separator row while ensuring the number of columns match the header
  -- and each cell has at least 3 dashes
  sep <- sepBy1
          (inlineSpace *> atLeast 3 '-')
          (optional (is '\n') *> inlineSpace *> is '|')
  checkNCol nCol sep


---- Main Parser ----

-- Reverse the input string
reverseInput :: Parser String
reverseInput = Parser $ \input -> Result (reverse input) ""

-- Trim the input by removing leading and trailing whitespaces
trim :: Parser String
trim = spaces *> reverseInput <* spaces *> reverseInput

-- Consumes newlines and returns the number of newlines consumed
-- This is to allow for empty p tags if there are 2 or more newlines in a row
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


---- Conversion ----

getTime :: IO String
getTime = formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%S" <$> getCurrentTime

-- Splits the input string into a list of strings by newline
-- Adds n indentations to each line
-- Joins the list of strings back into a single string with newlines
indent :: Int -> String -> String
indent n = unlines . map (replicate n ' ' ++) . lines

-- Surrounds the input string with the given tag
tag :: String -> String -> String
tag t c = "<" ++ t ++ ">" ++ c ++ "</" ++ t ++ ">"

-- Surrounds the input string with the given tag and adds indentation
block :: String -> String -> String
block t c = indent 4 ("<" ++ t ++ ">") ++ indent 4 c
            ++ indent 4 ("</" ++ t ++ ">")

-- Allows the user to specify a title for the HTML document
convertADTHTMLwithTitle :: String -> ADT -> String
convertADTHTMLwithTitle title (Markdown adt) = convertMarkdown (Just title) adt
convertADTHTMLwithTitle _ _ = "Invalid Input"

-- Converts the ADT to a HTML string
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

-- concatMap convertADTHTML adt is used to map convertADTHTML to each element in the list (adt)
-- The output string is then wrapped in the HTML tags

convertMarkdown :: Maybe String -> [ADT] -> String
convertMarkdown maybeTitle adt =
  let title = fromMaybe "Test" maybeTitle -- Default title is "Test"
  in "<!DOCTYPE html>\n<html lang=\"en\">\n\n<head>\n    <meta charset=\"UTF-8\">\n    <title>"
     ++ title
     ++ "</title>\n</head>\n\n<body>\n"
     ++ concatMap convertADTHTML adt
     ++ "</body>\n\n</html>\n"

convertNewline :: Int -> String
convertNewline n
  | n == 1 = ""
  | otherwise = indent 4 (tag "p" "") -- Empty p tag for 2 or more newlines

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
convertImage alt src title = indent 4 ("<img src=\"" ++ src
                              ++ "\" alt=\"" ++ alt ++ "\" title=\""
                              ++ title ++ "\">")

convertFootnoteReference :: Int -> String -> String
convertFootnoteReference n reference = indent 4 ("<p id=\"fn" ++ show n
                                        ++ "\">" ++ reference ++ "</p>")

convertFreeText :: [ADT] -> String
convertFreeText adt = indent 4 (tag "p" (concatMap convertADTHTML adt))

convertHeading :: Int -> [ADT] -> String
convertHeading n adt = indent 4
                        (tag ("h" ++ show n) (concatMap convertADTHTML adt))

convertBlockquote :: [ADT] -> String
convertBlockquote adt = block "blockquote" (concatMap convertADTHTML adt)

-- Optional language handled with pattern matching
convertCode :: String -> String -> String
convertCode "" c = replicate 4 ' '
                    ++ tag "pre" ("<code>" ++ c ++ "</code>") ++ "\n"
convertCode language c = replicate 4 ' ' ++ tag "pre" ("<code class=\"language-"
                          ++ language ++ "\">" ++ c ++ "</code>") ++ "\n"

-- List items are wrapped in a li tag
convertItem :: [ADT] -> String
convertItem adt = indent 4 (tag "li" (concatMap convertADTHTML adt))

-- The list items are first wrapped in a li tag, then the list is wrapped in an ol tag
convertOrderedList :: [ADT] -> String
convertOrderedList adt = block "ol" (concatMap convertADTHTML adt)

-- Wraps table around the header and body
convertTable :: ADT -> ADT -> String
convertTable headerRows bodyRows = block "table" (convertADTHTML headerRows
                                    ++ convertADTHTML bodyRows)

-- Wraps each cell in either a `th` or `td` tag based on the input
convertCell :: String -> [ADT] -> String
convertCell t c = indent 4 (tag t (concatMap convertADTHTML c))

-- Wraps the cells in a tr tag to form a row
convertRow :: String -> [[ADT]] -> String
convertRow t r = block "tr" (concatMap (convertCell t) r)

-- Pass `th` into convertRow to wrap the header cells in `th` tags,
-- then wrapped in a `tr` tag to form the header of the table
convertHeader :: [[ADT]] -> String
convertHeader = convertRow "th"

-- Pass `td` into convertRow to wrap the body cells in `td` tags,
-- then concatenate the wrapped rows to form the body of the table
convertBody :: [[[ADT]]] -> String
convertBody = concatMap (convertRow "td")
