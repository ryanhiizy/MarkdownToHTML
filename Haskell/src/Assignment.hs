module Assignment (markdownParser, convertADTHTML, getTime, convertADTHTMLwithTitle) where

import Control.Applicative (Alternative (..), Applicative (liftA2), liftA3, optional)
import Control.Monad (guard, void)
import Data.Foldable (asum)
import Data.Time.Clock (getCurrentTime)
import Data.Time.Format (defaultTimeLocale, formatTime)
import Instances (ParseError (..), ParseResult (..), Parser (..))
import Parser (atLeast, charTok, inlineSpace, is, isEnd, isNot, isNotWhitespace, positiveInt, lookAhead, manyCharTill, sepBy1, someCharTill, space, spaces, string, unexpectedStringParser, someTill)
import Data.Maybe (fromMaybe)

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
  | Item [ADT] [ADT]
  | OrderedList [ADT]
  | Header [[ADT]]
  | Body [[[ADT]]]
  | Table ADT ADT
  | NewLine Char
  | Markdown [ADT]
  deriving (Show, Eq)

-- $setup
-- >>> import Instances (isErrorResult, parse)


---- Text Modifiers ----
between :: String -> Parser String
between = liftA2 (*>) openingTag (someCharTill . string)

betweenTwo :: String -> String -> Parser String
betweenTwo opening closing = openingTag opening *> someCharTill (string closing)

try :: Parser a -> Parser (Maybe a)
try p = Parser $ \input -> case parse p input of
  Result rest a -> Result rest (Just a)
  Error _ -> Result input Nothing

-- >>> parse (notFollowedBy (string "abc")) "absc"
-- Result >absc< ()
notFollowedBy :: Parser a -> Parser ()
notFollowedBy p = do
  result <- try p
  case result of
    Just _ -> unexpectedStringParser "Unexpected string"
    Nothing -> return ()


text' :: String -> Parser Char
text' a = Parser f
  where
    f "" = Error UnexpectedEof
    f (x : _) | x `elem` a = Error $ UnexpectedString "Break character found"
    f input@(x : xs) = case parse (asum modifiers) input of
      Result _ _ -> Error $ UnexpectedString "Text modifiers are not allowed in free text"
      Error _ -> Result xs x

text :: String -> Parser ADT
text a = Text <$> some (text' a)

openingTag :: String -> Parser String
openingTag t = do
  result <- string t
  _ <- lookAhead (isNot (head t)) -- Prevent parsing cases such as "***a**"
  return result

r :: String -> Parser ADT
r t = Text <$> some (Parser f)
  where
    f "" = Error UnexpectedEof
    f (x : _) | x == '\n' = Error $ UnexpectedString "Newline found"
    f input@(x : xs) = case parse (string t) input of
      Result _ _ -> Error $ UnexpectedString "Closing tag found"
      Error _ -> Result xs x

-- >>> parse (nestedBetween "**") "**_ryan_ryan**"
-- Result >< [Italic [Text "ryan"],Text "ryan"]
nestedBetween :: String -> Parser [ADT]
nestedBetween t = do
  _ <- string t
  _ <- lookAhead (isNot (head t)) -- Prevent parsing cases such as "***a**"
  someTill (asum modifiers <|> r t) (string t)

-- >>> parse (nestedBetween "**") "**_ryan_ryan**"
-- Result >< [Italic [Text "ryan"],Text "ryan"]
nestedbetweenTwo :: String -> String -> Parser [ADT]
nestedbetweenTwo t1 t2 = do
  _ <- string t1
  _ <- lookAhead (isNot (head t1)) -- Prevent parsing cases such as "***a**"
  someTill (asum modifiers <|> r t2) (string t2)

modifiers :: [Parser ADT]
modifiers = [italic, bold, strikethrough, link, inlineCode, footnote]

-- >>> parse italic "_**ryan**_"
-- Result >< Italic [Bold [Text "ryan"]]
italic :: Parser ADT
italic = Italic <$> nestedBetween "_"

-- >>> parse bold "**_ryan_**"
-- Result >< Bold [Italic [Text "ryan"]]
bold :: Parser ADT
bold = Bold <$> nestedBetween "**"

-- >>> parse strikethrough "~~_ryan_~~"
-- Result >< Strikethrough [Italic [Text "ryan"]]
strikethrough :: Parser ADT
strikethrough = Strikethrough <$> nestedBetween "~~"

-- >>> parse link "[**ryan**](https://ryan.com)"
-- Result >< Link [Bold [Text "ryan"]] "https://ryan.com"
link :: Parser ADT
link = liftA2 Link (nestedbetweenTwo "[" "]") (inlineSpace *> betweenTwo "(" ")")

-- >>> parse inlineCode "`ryan`"
-- Result >< InlineCode "ryan"
inlineCode :: Parser ADT
inlineCode = InlineCode <$> between "`"

-- >>> parse footnote' "[^1 ]"
footnote' :: Parser Int
footnote' = inlineSpace *> string "[^" *> positiveInt <* is ']'

footnote :: Parser ADT
footnote = Footnote <$> footnote'

-- | -------------------------------------------------
-- | -------------------- Image ----------------------
-- | -------------------------------------------------

-- can there be whitespace between ] and (?
-- what about whitespace between " and )?
-- >>> parse image "![Alt text](/path/to/img.jpg \"Optional title\")"
-- Result >< Image "Alt text" "/path/to/img.jpg" "Optional title"
image :: Parser ADT
image =
  liftA3
    Image
    (inlineSpace *> is '!' *> isNotWhitespace *> betweenTwo "[" "]")
    (inlineSpace *> betweenTwo "(" " \"")
    (someCharTill (is '"') <* is ')')

-- | -------------------------------------------------
-- | -------------- Footnote Reference ---------------
-- | -------------------------------------------------

-- >>> parse footnoteReference "[^1]: My reference.\n[^2]:Another reference.\n[^3]:  The 2 spaces after the colon should be ignored"
-- >>> parse footnoteReference "[^1]: My reference."
-- Result >
-- [^2]:Another reference.
-- [^3]:  The 2 spaces after the colon should be ignored< FootnoteReference 1 "My reference."
-- Result >< FootnoteReference 1 "My reference."
footnoteReference :: Parser ADT
footnoteReference = liftA2 FootnoteReference footnote' (charTok ':' *> some (isNot '\n'))

-- | -------------------------------------------------
-- | ------------------ Free Text --------------------
-- | -------------------------------------------------
-- modifiers :: [Parser ADT]
-- modifiers = [italic, bold, strikethrough, link, inlineCode, footnote]

-- text' :: String -> Parser Char
-- text' a = Parser f
--   where
--     f "" = Error UnexpectedEof
--     f (x : _) | x `elem` a = Error $ UnexpectedString "Break character found"
--     f input@(x : xs) = case parse (asum modifiers) input of
--       Result _ _ -> Error $ UnexpectedString "Text modifiers are not allowed in free text"
--       Error _ -> Result xs x

-- >>> parse text "Here is some **markdown**"
-- Result >**markdown**< Text "Here is some "
-- text :: String -> Parser ADT
-- text a = Text <$> some (text' a)

freeText' :: String -> Parser [ADT]
freeText' a = some (text a <|> asum modifiers)

-- >>> parse freeText "_hey_ Here is some *markdown**"
-- Result >< FreeText [Italic "hey",Text " Here is some *markdown**"]
freeText :: Parser ADT
freeText = FreeText <$> freeText' "\n"

-- | -------------------------------------------------
-- | ------------------- Heading ---------------------
-- | -------------------------------------------------
checkHeadingSep :: Parser Int
checkHeadingSep = do
  seps <- atLeast 2 '=' <|> atLeast 2 '-'
  _ <- inlineSpace *> isEnd
  return $ if head seps == '=' then 1 else 2

checkHash :: Parser Int
checkHash = do
  hashes <- inlineSpace *> some (is '#')
  guard (length hashes <= 6) <|> unexpectedStringParser hashes
  return $ length hashes

heading :: Parser ADT
heading = liftA2 Heading checkHash (space *> freeText' "\n") <|> flip Heading <$> (freeText' "\n" <* charTok '\n') <*> checkHeadingSep

-- | -------------------------------------------------
-- | ------------------ Blockquote -------------------
-- | -------------------------------------------------

-- >>> parse blockquote "> This is a block quote.\n> It can **span** multiple lines.\n"
-- Result >< Blockquote [FreeText [Text "This is a block quote."],FreeText [Text "It can ",Bold "span",Text " multiple lines."]]
blockquote :: Parser ADT
blockquote = Blockquote <$> some (inlineSpace *> charTok '>' *> freeText <* optional (is '\n'))

-- | -------------------------------------------------
-- | --------------------- Code ----------------------
-- | -------------------------------------------------

-- does the closing ``` have to be in a new line?
-- >>> parse code "```haskell\nblockquote\n``````\nblockquote\n```\n"
-- >>> parse code "```haskell\nmain :: IO ()\nmain = do\n    putStrLn \"Never gonna give you up\"\n    putStrLn \"Never gonna let you down\"\n    putStrLn \"Never gonna run around and desert you\"\n```\n"
-- Result >
-- < Code "haskell" "blockquote\n``````\nblockquote"
-- Result >
-- < Code "haskell" "main :: IO ()\nmain = do\n    putStrLn \"Never gonna give you up\"\n    putStrLn \"Never gonna let you down\"\n    putStrLn \"Never gonna run around and desert you\""
code :: Parser ADT
code = do
  language <- inlineSpace *> openingTag "```" *> manyCharTill (is '\n')
  body <- someCharTill (string "\n```" <* isEnd)
  return $ Code language body

-- code = liftA2 Code (inlineSpace *> openingTag "```" *> manyTill (isNot '\n') (string "\n```")) (content "\n```" <* (eof <|> is '\n' $> ()))

-- | -------------------------------------------------
-- | ----------------- Ordered List ------------------
-- | -------------------------------------------------
nestedList :: Int -> Parser ADT
nestedList n = string (replicate (n + 4) ' ') *> orderedList' (n + 4)

-- need to check > 1?
listContent :: Int -> Parser ADT
listContent n = string (replicate n ' ') *> positiveInt *> string ". " *> (flip Item [] <$> freeText' "\n")

list :: Int -> Parser ADT
list = (is '\n' *>) . liftA2 (<|>) nestedList listContent

orderedList' :: Int -> Parser ADT
orderedList' n = do
  number <- positiveInt
  guard (number == 1) <|> unexpectedStringParser "Number must be 1"
  headList <- string ". " *> freeText' "\n"
  restList <- many (list n)
  return $ processRestList restList headList
  where
    processRestList (OrderedList nested : xs) headList = OrderedList (Item headList [OrderedList nested] : xs)
    processRestList xs headList = OrderedList (Item headList [] : xs)

-- >>> parse orderedList "1. Item 1\n    1. Sub Item 1\n    2. Sub Item 2\n    3. Sub Item 3\n2. **Bolded** Item 2\n6. Item 3\n7. Item 4"
-- Result >< OrderedList [Item [Text "Item 1"] [OrderedList [Item [Text "Sub Item 1"] [],Item [Text "Sub Item 2"] [],Item [Text "Sub Item 3"] []]],Item [Bold [Text "Bolded"],Text " Item 2"] [],Item [Text "Item 3"] [],Item [Text "Item 4"] []]
orderedList :: Parser ADT
orderedList = isNotWhitespace *> orderedList' 0

-- | -------------------------------------------------
-- | -------------------- Table ----------------------
-- | -------------------------------------------------

-- >>> parse cell "a |"
-- Result > |< Text "a"
cell :: Parser ADT
cell = Text <$> some (lookAhead (inlineSpace *> isNot '|') *> is ' ' <|> text' "|\n ")

-- >>> parse row "| Tables    **bold**    |Are| Cool  |\n| ------------- | ------------- | ----- |\n|"
-- Result >| ------------- | ------------- | ----- |
-- |< [[Text "Tables    ",Bold [Text "bold"]],[Text "Are"],[Text "Cool"]]
row :: Parser [[ADT]]
row = sepBy1 (some (asum modifiers <|> cell)) (inlineSpace *> charTok '|')

checkNCol :: Int -> [a] -> Parser [a]
checkNCol nCol values = do
  guard (length values == nCol) <|> unexpectedStringParser "Number of columns in table header and row do not match"
  return values

-- >>> parse (checkTableSep 3) "| ------------- | ------------- | ----- |\n|"
-- Result >|< ["---","---","---"]
checkTableSep :: Int -> Parser [String]
checkTableSep nCol = do
  sep <- sepBy1 (inlineSpace *> atLeast 3 '-') (inlineSpace *> charTok '|')
  checkNCol nCol sep

-- >>> parse table "  | Tables    **bold**    |Are| Cool  |\n| ------------- | ------------- | ----- |\n| here          | is            | data  |\n| here          | is            | data  |\n| here | is also | **bolded data** |\n| also | part of the | table |"
-- Result >< Table (Header [[Text "Tables    ",Bold [Text "bold"]],[Text "Are"],[Text "Cool"]]) (Body [[[Text "here"],[Text "is"],[Text "data"]],[[Text "here"],[Text "is"],[Text "data"]],[[Text "here"],[Text "is also"],[Bold [Text "bolded data"]]],[[Text "also"],[Text "part of the"],[Text "table"]]])
table :: Parser ADT
table = do
  header <- inlineSpace *> row
  _ <- checkTableSep (length header)
  body <- Body <$> some (row >>= checkNCol (length header))
  return $ Table (Header header) body

-- | -------------------------------------------------
-- | ------------------ Conversion -------------------
-- | -------------------------------------------------
newline :: Parser ADT
newline = NewLine <$> charTok '\n'

markdownElement :: Parser [ADT]
markdownElement = some (asum ([newline, image, footnoteReference, heading, blockquote, code, orderedList, table] ++ modifiers ++ [freeText]))

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
tag t c = "<" ++ t ++ ">" ++ c ++ "</" ++ t ++ ">"

block :: String -> String -> String
block t c = indent 4 ("<" ++ t ++ ">") ++ indent 4 c ++ indent 4 ("</" ++ t ++ ">")

-- Helper to indent content by `n` spaces and add a newline at the end
indent :: Int -> String -> String
indent n = unlines . map (replicate n ' ' ++) . lines

convertADTHTMLwithTitle :: String -> ADT -> String
convertADTHTMLwithTitle title (Markdown adt) = convertMarkdown (Just title) adt
convertADTHTMLwithTitle _ _ = "Invalid Input"

convertADTHTML :: ADT -> String
convertADTHTML (Markdown adt) = convertMarkdown Nothing adt
convertADTHTML (NewLine _) = ""
convertADTHTML (Italic i) = convertItalic i
convertADTHTML (Bold b) = convertBold b
convertADTHTML (Strikethrough s) = convertStrikethrough s
convertADTHTML (Link l url) = convertLink l url
convertADTHTML (InlineCode inline) = convertInlineCode inline
convertADTHTML (Footnote n) = convertFootnote n
convertADTHTML (Image alt src title) = convertImage alt src title
convertADTHTML (FootnoteReference n reference) = convertFootnoteReference n reference
convertADTHTML (Text t) = t
convertADTHTML (FreeText adt) = convertFreeText adt
convertADTHTML (Heading n adt) = convertHeading n adt
convertADTHTML (Blockquote adt) = convertBlockquote adt
convertADTHTML (Code language c) = convertCode language c
convertADTHTML (Item adt nested) = convertItem adt nested
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

convertItalic :: [ADT] -> String
convertItalic = tag "em" . concatMap convertADTHTML

convertBold :: [ADT] -> String
convertBold = tag "strong" . concatMap convertADTHTML

convertStrikethrough :: [ADT] -> String
convertStrikethrough = tag "del" . concatMap convertADTHTML

convertLink :: [ADT] -> String -> String
convertLink adt url = "<a href=\"" ++ url ++ "\">" ++ concatMap convertADTHTML adt ++ "</a>\n"

convertInlineCode :: String -> String
convertInlineCode = tag "code"

convertFootnote :: Int -> String
convertFootnote n = "<sup><a id=\"fn" ++ show n ++ "ref\" href=\"#fn" ++ show n ++ "\">" ++ show n ++ "</a></sup>\n"

convertImage :: String -> String -> String -> String
convertImage alt src title = indent 4 (tag "p" ("<img src=\"" ++ src ++ "\" alt=\"" ++ alt ++ "\" title=\"" ++ title ++ "\">"))

convertFootnoteReference :: Int -> String -> String
convertFootnoteReference n reference = indent 4 ("<p id=\"fn" ++ show n ++ "\">" ++ reference ++ "</p>")

convertFreeText :: [ADT] -> String
convertFreeText adt = indent 4 (tag "p" (concatMap convertADTHTML adt))

convertHeading :: Int -> [ADT] -> String
convertHeading n adt = indent 4 (tag ("h" ++ show n) (concatMap convertADTHTML adt))

convertBlockquote :: [ADT] -> String
convertBlockquote adt = block "blockquote" (concatMap convertADTHTML adt)

convertCode :: String -> String -> String
convertCode language c = replicate 4 ' ' ++ tag "pre" ("<code class=\"language-" ++ language ++ "\">" ++ c ++ "</code>") ++ "\n"

convertItem :: [ADT] -> [ADT] -> String
convertItem adt [] = indent 4 (tag "li" (concatMap convertADTHTML adt))
convertItem adt nested = indent 4 (tag "li" (concatMap convertADTHTML adt ++ "\n" ++ concatMap convertADTHTML nested))

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
