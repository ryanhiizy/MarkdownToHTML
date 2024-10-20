{-# OPTIONS_GHC -Wno-missing-export-lists #-}

-- | Implementation of a parser-combinator.
module Parser where

import Control.Applicative
import Control.Monad (replicateM, void)
import Data.Char
  ( isAlpha,
    isDigit,
    isLower,
    isSpace,
    isUpper,
  )
import Data.Functor (($>))
import Instances
  ( ParseError (..),
    ParseResult (..),
    Parser (..),
    readInt,
  )

-- $setup
-- >>> import Instances (isErrorResult, parse)

-- | -------------------------------------------------
-- | --------------- Core parsers --------------------
-- | -------------------------------------------------

-- | Return a parser that always fails with the given error.
--
-- >>> isErrorResult (parse (failed UnexpectedEof) "abc")
-- True
failed :: ParseError -> Parser a
failed = Parser . const . Error

-- | Produces a parser that always fails with 'UnexpectedChar' using the given
-- character.
unexpectedCharParser :: Char -> Parser a
unexpectedCharParser = Parser . const . Error . UnexpectedChar

-- | Return a parser that succeeds with a character off the input or fails with
-- an error if the input is empty.
--
-- >>> parse char "abc"
-- Result >bc< 'a'
--
-- >>> isErrorResult (parse char "")
-- True
char :: Parser Char
char = Parser f
  where
    f "" = Error UnexpectedEof
    f (x : xs) = Result xs x

-- | Parse numbers as int until non-digit

---- >>> parse int "abc"
-- Result >bc< 'a'
--
-- >>> isErrorResult (parse int "")
-- True
--
-- >>> isErrorResult (parse int "a")
-- True
int :: Parser Int
int = Parser f
  where
    -- This is okay because the case statement is small
    f "" = Error UnexpectedEof
    f x = case readInt x of
      Just (v, rest) -> Result rest v
      Nothing -> Error $ UnexpectedChar (head x)

-- | Write a parser that asserts that there is no remaining input.
--
-- >>> parse eof ""
-- Result >< ()
--
-- >>> isErrorResult (parse eof "abc")
-- True
eof :: Parser ()
eof = Parser f
  where
    f "" = Result "" ()
    f x = Error $ ExpectedEof x

-- | -------------------------------------------------
-- | --------------- Satisfy parsers -----------------
-- | -------------------------------------------------
-- | All of these parsers use the `satisfy` parser!
-- | Return a parser that produces a character but fails if:
--
--   * the input is empty; or
--
--   * the character does not satisfy the given predicate.
--
-- >>> parse (satisfy isUpper) "Abc"
-- Result >bc< 'A'
--
-- >>> isErrorResult (parse (satisfy isUpper) "abc")
-- True
satisfy :: (Char -> Bool) -> Parser Char
satisfy f = char >>= f'
  where
    -- This is okay because guards are small
    f' c
      | f c = pure c
      | otherwise = unexpectedCharParser c

-- | Return a parser that produces the given character but fails if:
--
--   * the input is empty; or
--
--   * the produced character is not equal to the given character.
--
-- >>> parse (is 'c') "c"
-- Result >< 'c'
--
-- >>> isErrorResult (parse (is 'c') "")
-- True
--
-- >>> isErrorResult (parse (is 'c') "b")
-- True
is :: Char -> Parser Char
is = satisfy . (==)

-- | Return a parser that produces any character but fails if:
--
--   * the input is empty; or
--
--   * the produced character is equal to the given character.
--
-- >>> parse (isNot 'c') "b"
-- Result >< 'b'
--
-- >>> isErrorResult (parse (isNot 'c') "")
-- True
--
-- >>> isErrorResult (parse (isNot 'c') "c")
-- True
isNot :: Char -> Parser Char
isNot = satisfy . (/=)

-- | Write a function that parses one of the characters in the given string.
--
-- /Hint/: What does `elem` do? What are its parameters?
--
-- >>> parse (oneof "abc") "bcdef"
-- Result >cdef< 'b'
--
-- >>> isErrorResult (parse (oneof "abc") "def")
-- True
oneof :: String -> Parser Char
oneof = satisfy . flip elem

-- | Write a function that parses any character, but fails if it is in the
-- given string.
--
-- /Hint/: What does `notElem` do? What are its parameters?
--
-- >>> parse (noneof "bcd") "abc"
-- Result >bc< 'a'
--
-- >>> isErrorResult (parse (noneof "abcd") "abc")
-- True
noneof :: String -> Parser Char
noneof = satisfy . flip notElem

-- | Return a parser that produces a character between '0' and '9' but fails if
--
--   * the input is empty; or
--
--   * the produced character is not a digit.
--
-- /Hint/: Use the 'isDigit' function
digit :: Parser Char
digit = satisfy isDigit

-- | Return a parser that produces a space character but fails if
--
--   * the input is empty; or
--
--   * the produced character is not a space.
--
-- /Hint/: Use the 'isSpace' function
space :: Parser Char
space = satisfy isSpace

-- | Return a parser that produces a lower-case character but fails if:
--
--   * the input is empty; or
--
--   * the produced character is not lower-case.
--
-- /Hint/: Use the 'isLower' function
lower :: Parser Char
lower = satisfy isLower

-- | Return a parser that produces an upper-case character but fails if:
--
--   * the input is empty; or
--
--   * the produced character is not upper-case.
--
-- /Hint/: Use the 'isUpper' function
upper :: Parser Char
upper = satisfy isUpper

-- | Return a parser that produces an alpha character but fails if:
--
--   * the input is empty; or
--
--   * the produced character is not alpha.
--
-- /Hint/: Use the 'isAlpha' function
alpha :: Parser Char
alpha = satisfy isAlpha

-- | Write a parser that will parse zero or more spaces (including newlines)
--
-- /Hint/: Remember the `space` parser!
--
-- >>> parse spaces " abc"
-- Result >abc< " "
--
-- >>> parse spaces "abc"
-- Result >abc< ""
spaces :: Parser String
spaces = many space

-- | Return a parser that produces one or more space characters (consuming
-- until the first non-space) but fails if:
--
--   * the input is empty; or
--
--   * the first produced character is not a space.
--
-- /Hint/: Remember the `space` parser!
--
-- >>> parse spaces1 " abc"
-- Result >abc< " "
--
-- >>> isErrorResult $ parse spaces1 "abc"
-- True
spaces1 :: Parser String
spaces1 = some space

-- | Write a parser that will parse zero or more spaces (not including newlines)
--
-- The possible whitespace characters: \t, \r, \f, \v, and a space character.
--
-- >>> parse inlineSpace " abc"
-- Result >abc< " "
--
-- >>> parse inlineSpace "abc"
-- Result >abc< ""
inlineSpace :: Parser String
inlineSpace = many (oneof "\t\r\f\v ")

-- | Write a function that parses the given string (fails otherwise).
--
-- /Hint/: Use 'is' and 'traverse'.
--
-- >>> parse (string "abc") "abcdef"
-- Result >def< "abc"
--
-- >>> isErrorResult (parse (string "abc") "bcdef")
-- True
string :: String -> Parser String
string = traverse is

-- | -------------------------------------------------
-- | --------------- Token parsers -------------------
-- | -------------------------------------------------

-- | Write a function that applies the given parser, then parses 0 or more
-- spaces, then produces the result of the original parser.
--
-- /Hint/: You can use the Monad instance or Applicatives
--
-- >>> parse (tok (is 'a')) "a bc"
-- Result >bc< 'a'
--
-- >>> parse (tok (is 'a')) "abc"
-- Result >bc< 'a'
tok :: Parser a -> Parser a
tok = (<* spaces)

-- tok p = do
--   r <- p
--   spaces
--   pure r

-- | Write a function that parses the given char followed by 0 or more spaces.
--
-- /Hint/: Remember the `is` parser
--
-- >>> parse (charTok 'a') "abc"
-- Result >bc< 'a'
--
-- >>> isErrorResult (parse (charTok 'a') "dabc")
-- True
charTok :: Char -> Parser Char
charTok = tok . is

-- | Write a parser that parses a comma ',' followed by 0 or more spaces.
--
-- /Hint/: We just implemented `charTok`
--
-- >>> parse commaTok ",123"
-- Result >123< ','
--
-- >>> isErrorResult( parse commaTok "1,23")
-- True
commaTok :: Parser Char
commaTok = charTok ','

-- | Write a function that parses the given string, followed by 0 or more
-- spaces.
--
-- /Hint/: Remember the `string` parser
--
-- >>> parse (stringTok "abc") "abc  "
-- Result >< "abc"
--
-- >>> isErrorResult (parse (stringTok "abc") "bc  ")
-- True
stringTok :: String -> Parser String
stringTok = tok . string


---- Additional Parsers ----

-- | Returns an unexpected string error wrapped in a parser
unexpectedStringParser :: String -> Parser a
unexpectedStringParser = Parser . const . Error . UnexpectedString

-- | Executes parser `p` until parser `end` succeeds zero or more times
--
-- >>> parse (manyTill (is 'a') (is 'b')) "aaaab"
-- Result >< "aaaa"
-- >>> parse (manyTill (is 'a') (is 'b')) "b"
-- Result >< ""
manyTill :: Parser a -> Parser b -> Parser [a]
manyTill p end = manyTill'
  where
    -- Tries to parse the end parser, if it fails,
    -- it will recursively parse the p parser to build the list
    manyTill' = (end $> []) <|> liftA2 (:) p manyTill'

-- | Executes parser `p` until parser `end` succeeds one or more times
--
-- >>> isErrorResult (parse (someTill (is 'a') (is 'b')) "b")
-- True
someTill :: Parser a -> Parser b -> Parser [a]
someTill p end = liftA2 (:) p (manyTill p end)

-- | Parses all characters until the given parser succeeds zero or more times
manyCharTill :: Parser b -> Parser String
manyCharTill = manyTill char

-- | Parses all characters until the given parser succeeds one or more times
someCharTill :: Parser b -> Parser String
someCharTill = someTill char

-- | Executes the given parser and return the result without consuming the input
--
-- >>> parse (lookAhead (is 'a')) "abc"
-- Result >abc< 'a'
lookAhead :: Parser a -> Parser a
lookAhead (Parser p) = Parser f
  where
    f input = case p input of
      Result _ c -> Result input c -- Returns the result while keeping the input
      Error e -> Error e

-- | Modified version of the traditional `sepBy1` that ensures the values are surrounded by the separator
--
-- >>> parse (sepBy1 (string "abc") (is '|')) "|abc|abc|abc|"
-- Result >< ["abc","abc","abc"]
sepBy1 :: Parser a -> Parser b -> Parser [a]
sepBy1 p sep = some (sep *> p) <* sep

-- | Ensures that the next character is a newline or the end of the file without consuming the newline
--
-- >>> parse isEnd ""
-- Result >< ()
-- >>> parse isEnd "\n"
-- Result >
-- < ()
isEnd :: Parser ()
isEnd = eof <|> void (lookAhead (is '\n'))

-- | Ensures that the next character is not a whitespace without consuming it
--
-- >>> parse isNotWhitespace "a"
-- Result >a< 'a'
-- >>> isErrorResult (parse isNotWhitespace " a")
-- True
isNotWhitespace :: Parser Char
isNotWhitespace = lookAhead (satisfy (not . isSpace))

-- | Only parses positive integers that do not start with a whitespace
--
-- >>> parse positiveInt "1"
-- Result >< 1
-- >>> isErrorResult (parse positiveInt "-1")
-- True
positiveInt :: Parser Int
-- As long as it does not have a '-' sign, the int parser will handle the rest
positiveInt = isNotWhitespace *> lookAhead (satisfy (/= '-')) *> int

-- | Parses at least n of a given character, consuming extra occurrences of the character
--
-- >>> parse (atLeast 3 'a') "aaaaab"
-- Result >b< "aaa"
atLeast :: Int -> Char -> Parser String
atLeast n c = replicateM n (is c) <* many (is c)
