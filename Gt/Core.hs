module Gt.Core
    (
      do_trans
    , do_trans_each_word
    )
where

import Gt.Net
import Gt.Langs
import Gt.Helpers
import Gt.Translation

import Text.JSON

do_trans :: Lang -> Lang -> String -> IO String
do_trans sl tl str =
  do
    jresp <- get_resp sl tl str
    return $ show $ jresp_to_resp $ jresp

do_trans_each_word :: IO String
do_trans_each_word = undefined

jresp_to_resp :: String -> Resp
jresp_to_resp jobj = foldr fill_resp br obj_list
    where obj_list = fromJSObject $ from_ok $ (decode jobj :: Result (JSObject JSValue))
          br = blank_resp

fill_resp :: (String, JSValue) -> Resp -> Resp
fill_resp (vclass, jval) resp =
    case vclass of
        "sentences" -> resp {sentences = jval_to_sentences jval}
        "dict"      -> resp {dicts = jval_to_dicts jval}
        "src"       -> resp {src = jstr_to_str jval}
        _           -> resp -- Ignoring any new data

-- Sentences processing
jval_to_sentences :: JSValue -> Sentences
jval_to_sentences (JSArray jval) = Sentences { sent_list = foldr jsso_to_sentence [] jval }
jval_to_sentences _              = undefined

jsso_to_sentence :: JSValue -> [Sentence] -> [Sentence]
jsso_to_sentence (JSObject jobj) acc =
    -- in the truth we need to reverse data,
    -- but currently there is only one element,
    -- so just leaving as is
    (foldr tot (Sentence "" "" "") in_list):acc
    where in_list = fromJSObject jobj
jsso_to_sentence _ _ = undefined

-- Conversion of inner list with trans-orig-translit data
tot :: (String, JSValue) -> Sentence -> Sentence
tot (tclass, jval) s =
    case tclass of
        "trans"    -> s { trans    = jstr_to_str jval}
        "orig"     -> s { orig     = jstr_to_str jval}
        "translit" -> s { translit = jstr_to_str jval}
        _          -> s

-- Dictionaries processing
jval_to_dicts :: JSValue -> Dicts
jval_to_dicts (JSArray jval) = Dicts { dict_list = foldr jsdo_to_dict [] jval }
jval_to_dicts _ = undefined

jsdo_to_dict :: JSValue -> [Dict] -> [Dict]
jsdo_to_dict (JSObject jobj) acc =
    -- in the truth we need to reverse data,
    -- but currently there is only one element,
    -- so just leaving as is
    (foldr pt (Dict "" []) in_list):acc
    where in_list = fromJSObject jobj
jsdo_to_dict _ _ = undefined

-- Conversion of inner list with pos-term data
pt :: (String, JSValue) -> Dict -> Dict
pt (tclass, jval) s =
    case tclass of
        "pos"      -> s { pos   = pos_to_str jval }
        "terms"    -> s { terms = nub_sort $ jlist_to_slist jval}
        _          -> s

pos_to_str :: JSValue -> String
pos_to_str jval =
    case p of
        "" -> "common"
        _  -> p
    where p = jstr_to_str jval
