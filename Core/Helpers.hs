module Core.Helpers
    (
        nub_sort
    ,   from_ok
    ,   jarr_to_list
    ,   jstr_to_str
    ,   jlist_to_slist
    )
where

import Text.JSON
import Text.JSON.String

import qualified Data.Set as S

-- nub + sort through Set
nub_sort :: Ord a => [a] -> [a]
nub_sort = S.toList . S.fromList

-- shifts lines for 4 spaces for n times
shift_lines :: Int -> String -> String
shift_lines n = unlines . map (replicate (n*4) ' ' ++) . lines


-- JSON deconstruct helpers
from_ok :: Result a -> a
from_ok (Ok x) = x

jarr_to_list :: JSValue -> [JSValue]
jarr_to_list (JSArray jarr) = jarr
jarr_to_list _              = []

jstr_to_str :: JSValue -> String
jstr_to_str (JSString jstr) = fromJSString jstr
jstr_to_str _               = ""

jlist_to_slist :: JSValue -> [String]
jlist_to_slist (JSArray xs) = map (jstr_to_str) xs
