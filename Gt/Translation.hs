module Gt.Translation
where

import Data.List
import Gt.Helpers

type Trans    = String
type Orig     = String
type Translit = String

data Sentence = Sentence {
      trans    :: Trans
    , orig     :: Orig
    , translit :: Translit
    }

data Sentences = Sentences {
      sent_list :: [Sentence]
    }

type Pos   = String
type Terms = [String]

data Dict = Dict {
      pos :: Pos
    , terms :: Terms
    }

data Dicts = Dicts {
      dict_list :: [Dict]
    }

type Src = String
data Resp = Resp {
      sentences :: Sentences
    , dicts :: Dicts
    , src :: Src
    }

instance Show Dict where
    show = ps_dict

instance Show Sentence where
    show = ps_sentence

instance Show Dicts where
    show = concat . intersperse "\n\n" . map show .  dict_list

instance Show Sentences where
    show = concatMap show . sent_list

instance Show Resp where
    show = ps_resp

ps_dict :: Dict -> String
ps_dict d = pos d
          ++ ":\n    "
          ++ concat (intersperse ", " $ terms d)

ps_sentence :: Sentence -> String
ps_sentence s = "Translation: "
              ++ trans s
              ++ "\nOriginal text: "
              ++ orig s
              ++ "\nTranslit: "
              ++ translit s
              ++ "\n\n"

ps_resp :: Resp -> String
ps_resp resp = shift_lines 1 $ "Source language: "
                             ++ (src resp)
                             ++ "\n\n" ++ (show $ sentences resp)
                             ++ (shift_lines 0 $ show $ dicts resp)

blank_resp :: Resp
blank_resp = Resp {sentences = Sentences {sent_list = []}, dicts = Dicts {dict_list = []}, src = ""}

{-
     Some crap for testing and tweaking
y  = Sentences [Sentence "DHT" "BitTorrent" "Old, old Mendelson"]
d  = Dict {pos = "noun", terms = ["google","brucks"]}
d2 = Dict {pos = "adjective", terms = ["serna","tripod"]}
ds = Dicts {dict_list = [d, d2, d, d2]}
r  = Resp {sentences = y, dicts = ds, src = "en"}
-}