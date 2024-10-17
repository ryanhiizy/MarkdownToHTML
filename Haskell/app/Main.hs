{-# LANGUAGE OverloadedStrings #-}

import Assignment (convertADTHTMLwithTitle, getTime, markdownParser)
import Control.Monad.Cont (liftIO)
import Data.Aeson (object, (.=))
import Data.Aeson.Key (fromString)
import Data.Text.Lazy (Text, pack, unpack)
import Data.Text.Lazy.Encoding (decodeUtf8)
import Instances (ParseResult (Result), parse)
import Web.Scotty (ActionM, body, json, post, scotty)

getResult :: ParseResult a -> (a -> String) -> String
getResult (Result _ a) f = f a
getResult _ _ = ""

-- Magic code to convert key, value pairs to JSON to send back to the server
jsonResponse :: [(String, String)] -> ActionM ()
jsonResponse pairs =
  json $ object [fromString key .= (pack value :: Text) | (key, value) <- pairs]

-- Helper function to parse the title and content from the request body
-- Splits the input string at the first newline character.
parseTitleAndContent :: String -> (String, String)
parseTitleAndContent str =
  let (title, content) = break (== '\n') str
   in (title, drop 1 content)

main :: IO ()
main = scotty 3000 $ do
  post "/api/convertMD" $ do
    requestBody <- body
    -- Convert the raw request body from ByteString to Text
    let requestBodyText = decodeUtf8 requestBody
        -- Convert the Text to String
        str = unpack requestBodyText
        -- Extract the title and content from the request body
        (title, content) = parseTitleAndContent str
        -- Parse the Markdown string using 'markdownParser' and apply 'convertADTHTMLwithTitle'
        converted_html = getResult (parse markdownParser content) (convertADTHTMLwithTitle title)

    -- Respond with the converted HTML as JSON
    jsonResponse [("html", converted_html)]


  post "/api/saveHTML" $ do
    requestBody <- body
    let html = unpack $ decodeUtf8 requestBody
    -- Get the current time as a string
    currentTime <- liftIO getTime
    -- Specify the directory and file name to save the HTML file
    let dir = "out"
    let fileName = dir ++ "/" ++ currentTime ++ ".html"
    -- Write the HTML to the file
    liftIO $ writeFile fileName html
