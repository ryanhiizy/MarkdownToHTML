{-# OPTIONS_GHC -Wno-missing-export-lists #-}

-- | Implementation of a parser-combinator.
module Parser where

import Control.Applicative
import Control.Monad (guard)
import Data.Char
  ( isAlpha,
    isDigit,
    isLower,
    isSpace,
    isUpper,
  )
import Data.List (isPrefixOf)
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

----------------------------
---- ADDITIONAL PARSERS ----
----------------------------

-- withoutAny :: [String] -> Parser String
-- withoutAny tags = Parser f
--   where
--     f "" = Error UnexpectedEof
--     f input@(x : xs) = case find (`isPrefixOf` input) tags of
--       Just tag
--         | (tag ++ tag) `isPrefixOf` input ->
--             let res = drop (length tag) input
--              in Result res tag
--         | tag `isPrefixOf` input && tag `isInfixOf` drop (length tag) input ->
--             Error $ UnexpectedString "Tag found"
--       _ -> Result xs [x]

-- withoutAny :: [String] -> Parser String
-- withoutAny tags = Parser f
--   where
--     f "" = Error UnexpectedEof
--     f input@(x : xs)
--       -- If a tag is found, stop and return the current result
--       | any (`isPrefixOf` input) tags = Result input ""
--       -- Otherwise, consume one character and continue
--       | otherwise =
--           let Parser g = withoutAny tags
--            in case g xs of
--                 Result rest res -> Result rest (x : res)
--                 Error err -> Error err

-- between :: String -> Parser String
-- between tag = do
--   -- Consume the opening tag
--   _ <- string tag
--   -- Parse the content, ensuring it’s not empty
--   content <- withoutAny [tag]
--   -- Fail if no content is found, i.e., it's empty
--   if null content
--     then failed $ UnexpectedString "Empty content between tags"
--     else do
--       -- Try to parse the closing tag
--       _ <- string tag
--       return content

-- between :: String -> Parser String
-- between tag = do
--   -- Consume the opening tag
--   _ <- string tag
--   -- Parse the content between the tags
--   content <- some (withoutAny [tag])
--   -- Consume the closing tag
--   _ <- string tag
--   return content

-- without :: String -> Parser Char
-- without tag = Parser f
--   where
--     f "" = Error UnexpectedEof
--     f input@(x : xs)
--       | tag `isPrefixOf` input = Error $ UnexpectedString "Tag found"
--       | otherwise = Result xs x

-- withoutAny :: [String] -> Parser Char
-- withoutAny tags = Parser f
--   where
--     f "" = Error UnexpectedEof
--     f input@(x : xs)
--       | any ((`isPrefixOf` input) . join (++)) tags = Result xs x
--       -- \tag -> tag `isPrefixOf` input && tag `isInfixOf` drop (length tag) input
--       | any (liftA2 (&&) (`isPrefixOf` input) (((`drop` input) . length) <**> isInfixOf)) tags =
--           Error $ UnexpectedString "Tag found"
--       | otherwise = Result xs x

-- | Produces a parser that always fails with 'UnexpectedChar' using the given
-- character.

-- -- Parse any character that isn't the start of any given tags
-- withoutAny :: [String] -> [Char] -> Parser Char
-- withoutAny tags starts = Parser f
--   where
--     f "" = Error UnexpectedEof
--     f input@(x : xs)
--       | x `elem` starts = Error $ UnexpectedString "Start found"
--       | any ((`isPrefixOf` input) . join (++)) tags = Result xs x
--       | isPrefixAndInfix input tags = Error $ UnexpectedString "Tag found"
--       | otherwise = Result xs x

-- -- Utility function to check if any string in a list is both a prefix and infix of the input
-- isPrefixAndInfix :: String -> [String] -> Bool
-- isPrefixAndInfix input = any (liftA2 (&&) (`isPrefixOf` input) (((`drop` input) . length) <**> isInfixOf))

unexpectedStringParser :: String -> Parser a
unexpectedStringParser = Parser . const . Error . UnexpectedString

-- Parser that fails if leading spaces (inline) are found
noLeadingSpaces :: Parser String
noLeadingSpaces = do
  whitespace <- inlineSpace
  guard (null whitespace) <|> unexpectedStringParser "Expected no leading spaces"
  return ""

-- Parser that parses a positive integer
positiveInt :: Parser Int
positiveInt = do
  n <- int
  guard (n > 0) <|> unexpectedStringParser "Expected a positive integer"
  return n

-- Parse any character that isn't the given tag
without :: String -> Parser Char
without tag = Parser f
  where
    f "" = Error UnexpectedEof
    f input@(x : xs)
      | tag `isPrefixOf` input = Error $ UnexpectedString "Tag found"
      | otherwise = Result xs x

between' :: String -> Parser String
between' tag = Parser f
  where
    f "" = Error UnexpectedEof
    f input = case parse (string tag) input of
      Result xs x
        | head xs == head tag -> Error $ UnexpectedString "Tag found"
        | otherwise -> Result xs x
      Error e -> Error e

between :: String -> Parser String
between tag = between' tag *> some (without tag) <* string tag

anyChar :: Parser Char
anyChar = Parser f
  where
    f "" = Error UnexpectedEof
    f (x : xs) = Result xs x

headingSepLength :: Parser Int
headingSepLength = do
  seps <- some (is '=') <|> some (is '-')
  guard (length seps >= 2) <|> unexpectedStringParser seps
  return $ if head seps == '=' then 1 else 2

hashLength :: Parser Int
hashLength = do
  hashes <- some (is '#')
  guard (length hashes <= 6) <|> unexpectedStringParser hashes
  return $ length hashes
