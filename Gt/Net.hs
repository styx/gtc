module Gt.Net
    (
      get_resp
    , get_resp_each_word
    )
where

import Gt.Langs
import Data.Maybe
import Network.URL
import Network.HTTP


trans_url :: Lang -> Lang -> String -> String
trans_url sl tl text =
    exportURL $ foldl add_param base_url [("text", text), ("sl", sl), ("tl", tl)]
        where base_url = fromJust $ importURL "http://translate.google.com/translate_a/t?client=t&pc=0&oc=1&hl=en&ie=UTF-8&oe=UTF-8"

get_resp :: Lang -> Lang -> String -> IO String
get_resp sl tl str =
  do result <- simpleHTTP (getRequest $ trans_url sl tl str) >>= getResponseBody
     return result

get_resp_each_word :: Lang -> Lang -> String -> IO String
get_resp_each_word = undefined
