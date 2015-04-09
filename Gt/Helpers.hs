module Gt.Helpers
    (
        decodeString
    ,   from_ok
    ,   jarr_to_list
    ,   jstr_to_str
    ,   jlist_to_slist
    ,   nub_sort
    ,   shift_lines
    )
where

import Data.Char
import Data.Word

import Text.JSON

import qualified Codec.Binary.UTF8.String as U (decode)
import qualified Data.Set as S

-- UTF-8 decode helpers
stringToBytes :: String -> [Word8]
stringToBytes = map (toEnum . ord)

decodeString :: String -> String
decodeString = U.decode . stringToBytes


-- nub + sort through Set
nub_sort :: Ord a => [a] -> [a]
nub_sort = S.toList . S.fromList


-- shifts lines for 4 spaces for n times
shift_lines :: Int -> String -> String
shift_lines n = unlines . map (replicate (n*4) ' ' ++) . lines


-- JSON deconstruct helpers
from_ok :: String -> Result a -> a
from_ok _raw_data (Ok x)    = x
from_ok raw_data (Error e) = error $ unlines [ "Gt.Helpers.from_ok:"
                                             , show e
                                             , "on input:"
                                             , show raw_data
                                             ]
jarr_to_list :: JSValue -> [JSValue]
jarr_to_list (JSArray jarr) = jarr
jarr_to_list _              = []

jstr_to_str :: JSValue -> String
jstr_to_str (JSString jstr) = fromJSString jstr
jstr_to_str _               = ""

jlist_to_slist :: JSValue -> [String]
jlist_to_slist (JSArray xs) = map jstr_to_str xs
jlist_to_slist _            = error "Gt.Helpers.jlist_to_slist: not a JSArray"
