module Core
    (
      do_trans
    , do_trans_each_word
    , langs
    )
where

import Data.List
import Data.Maybe
import Network.URL
import Network.HTTP

trans_url :: String -> String -> String -> String
trans_url sl tl text =
    exportURL $ foldl add_param base_url [("text", text), ("sl", sl), ("tl", tl)]
        where base_url = fromJust $ importURL "http://translate.google.com/translate_a/t?client=t&pc=0&oc=1&hl=en&ie=UTF-8&oe=UTF-8"

-- TODO: Need to add JSON parsing
do_trans :: String -> String -> String -> IO String
do_trans sl tl str =
  do result <- simpleHTTP (getRequest $ trans_url sl tl str) >>= getResponseBody
     return result

do_trans_each_word :: String -> String -> String -> IO String
do_trans_each_word sl tl str = undefined
--  do result <- simpleHTTP (getRequest $ trans_url sl tl word) >>= getResponseBody
--     return result

langs :: String
langs = "en ru be"