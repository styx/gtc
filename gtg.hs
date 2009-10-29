
module Main (
    main
) where

import Core
import Data.Word
import Data.Char
import Graphics.UI.Gtk
import Graphics.UI.Gtk.Glade
import Codec.Binary.UTF8.String (encode, decode)
import qualified System.IO.UTF8 as U


-- Encoding/Decoding helpers
bytesToString :: [Word8] -> String
bytesToString xs = map (chr . fromEnum) xs

stringToBytes :: String -> [Word8]
stringToBytes xs = map (toEnum . ord) xs

encodeString :: String -> String
encodeString xs = bytesToString (encode xs)

decodeString :: String -> String
decodeString xs = decode (stringToBytes xs)

main :: IO()
main = do
    initGUI
    Just xml  <- xmlNew "gui.glade"

    -- Casts
    window          <- xmlGetWidget xml castToWindow    "gtg_main"
    lbl_translation <- xmlGetWidget xml castToLabel     "lbl_translation"
    ent_trans_text  <- xmlGetWidget xml castToEntry     "ent_trans_text"
    btn_translate   <- xmlGetWidget xml castToButton    "btn_translate"
    btn_close       <- xmlGetWidget xml castToButton    "btn_close"
    cmb_dest_lang   <- xmlGetWidget xml castToComboBox  "cmb_dest_lang"
    cmb_source_lang <- xmlGetWidget xml castToComboBox  "cmb_source_lang"

    comboBoxSetModelText cmb_dest_lang
    comboBoxSetModelText cmb_source_lang

    mapM_ (comboBoxAppendText cmb_dest_lang  ) (words langs)
    mapM_ (comboBoxAppendText cmb_source_lang) (words langs)

    comboBoxSetActive cmb_source_lang 0
    comboBoxSetActive cmb_dest_lang   0

    -- Actions
    onClicked       btn_close      (widgetDestroy window)
    onClicked       btn_translate  (trans_click ent_trans_text lbl_translation cmb_source_lang cmb_dest_lang)
    onEntryActivate ent_trans_text (trans_click ent_trans_text lbl_translation cmb_source_lang cmb_dest_lang)
    onDestroy       window         mainQuit

    -- Let's go
    widgetShowAll window
    mainGUI

-- trans_click ::
trans_click ent lbl cmb_src cmb_dest = do
    text <- get ent entryText
    Just src <- comboBoxGetActiveText cmb_src
    Just dst <- comboBoxGetActiveText cmb_dest
    translated_text <- do_trans src dst text
    set lbl [ labelText := (decodeString translated_text) ]
