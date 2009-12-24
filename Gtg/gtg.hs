
module Main (
    main
) where

import Gt.Core
import Data.Word
import Data.Char
import Graphics.UI.Gtk
import Graphics.UI.Gtk.Glade
import Codec.Binary.UTF8.String (encode, decode)
import Paths_gt_tools(getDataDir)
import qualified System.IO.UTF8 as U


-- Encoding/Decoding helpers
bytesToString :: [Word8] -> String
bytesToString = map (chr . fromEnum)

stringToBytes :: String -> [Word8]
stringToBytes = map (toEnum . ord)

encodeString :: String -> String
encodeString = bytesToString . encode

decodeString :: String -> String
decodeString = decode . stringToBytes

main :: IO()
main = do
    initGUI
    data_dir  <- getDataDir
    Just xml  <- xmlNew $ data_dir ++ "/main.glade"

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

    mapM_ (comboBoxAppendText cmb_dest_lang  ) (tail langs_descrs)
    mapM_ (comboBoxAppendText cmb_source_lang) langs_descrs

    -- Actions
    onClicked       btn_close      (widgetDestroy window)
    onClicked       btn_translate  (trans_click ent_trans_text lbl_translation cmb_source_lang cmb_dest_lang)
    onEntryActivate ent_trans_text (trans_click ent_trans_text lbl_translation cmb_source_lang cmb_dest_lang)
    onDestroy       window         mainQuit

    -- Let's go
    widgetShowAll window
    mainGUI

get_dest_lang :: ComboBox -> IO (String)
get_dest_lang cmb = do
    index <- comboBoxGetActive cmb
    return $ tail langs !! index

get_src_lang :: ComboBox -> IO (String)
get_src_lang cmb = do
    index <- comboBoxGetActive cmb
    return (langs !! index)

-- trans_click ::
trans_click ent lbl cmb_src cmb_dest = do
    text <- get ent entryText
    src <- get_src_lang cmb_src
    dst <- get_dest_lang cmb_dest
    translated_text <- do_trans src dst text
    set lbl [ labelText := decodeString translated_text ]
