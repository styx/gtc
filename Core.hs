module Core
    (
      do_trans
    , do_trans_each_word
    , langs
    , langs_descrs
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

langs :: [String]
langs = words
        "auto af sq ar be bg ca zh-CN hr cs da nl en et tl fi fr gl de el iw\
         \ hi hu is id ga it ja ko lv lt mk ms mt no fa pl pt ro ru sr sk sl es\
         \ sw sv th tr uk vi cy yi"

langs_descrs :: [String]
langs_descrs = words
                "AutoDetect Afrikaans Albanian Arabic Belarusian Bulgarian \
                \ Catalan Chinese Croatian Czech Danish Dutch English Estonian \
                \ Filipino Finnish French Galician German Greek Hebrew Hindi \
                \ Hungarian Icelandic Indonesian Irish Italian Japanese Korean \
                \ Latvian Lithuanian Macedonian Malay Maltese Norwegian Persian \
                \ Polish Portuguese Romanian Russian Serbian Slovak Slovenian \
                \ Spanish Swahili Swedish Thai Turkish Ukrainian Vietnamese \
                \ Welsh Yiddish"