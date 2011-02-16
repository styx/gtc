
module Main (
    main
) where

import Gt.Core
import Gt.Langs
import Graphics.UI.Gtk
import Graphics.UI.Gtk.Glade
import Paths_gt_tools(getDataDir)
import Data.List (elemIndex)
import qualified System.Environment.UTF8 as U

comboIndex :: [String] -> String -> Int
comboIndex xs x = maybe (-1) (id) (x `elemIndex` xs)

main :: IO()
main = do
    -- Parse command line 
    args <- U.getArgs
    let fromL = if (length args) >= 1 then comboIndex langs (args !! 0) else -1
    let toL   = if (length args) >= 2 then comboIndex (tail langs) (args !! 1) else -1

    -- GUI initialization
    _ <- initGUI
    data_dir  <- getDataDir
    Just xml  <- xmlNew $ data_dir ++ "/Gtg/main.glade"

    -- Casts
    window          <- xmlGetWidget xml castToWindow    "gtg_main"
    lbl_translation <- xmlGetWidget xml castToLabel     "lbl_translation"
    ent_trans_text  <- xmlGetWidget xml castToEntry     "ent_trans_text"
    btn_translate   <- xmlGetWidget xml castToButton    "btn_translate"
    btn_close       <- xmlGetWidget xml castToButton    "btn_close"
    cmb_dest_lang   <- xmlGetWidget xml castToComboBox  "cmb_dest_lang"
    cmb_source_lang <- xmlGetWidget xml castToComboBox  "cmb_source_lang"

    _ <- comboBoxSetModelText cmb_dest_lang
    _ <- comboBoxSetModelText cmb_source_lang

    mapM_ (comboBoxAppendText cmb_dest_lang  ) (tail langs_descrs)
    mapM_ (comboBoxAppendText cmb_source_lang) langs_descrs

    -- Set active lang from command line
    comboBoxSetActive cmb_source_lang fromL
    comboBoxSetActive cmb_dest_lang toL

    -- Actions
    _ <- onClicked       btn_close      (widgetDestroy window)
    _ <- onClicked       btn_translate  (trans_click ent_trans_text lbl_translation cmb_source_lang cmb_dest_lang)
    _ <- onEntryActivate ent_trans_text (trans_click ent_trans_text lbl_translation cmb_source_lang cmb_dest_lang)
    _ <- onDestroy       window         mainQuit

    -- Let's go
    widgetShowAll window
    mainGUI

get_dest_lang :: ComboBox -> IO String
get_dest_lang cmb = do
    index <- comboBoxGetActive cmb
    return $ tail langs !! index

get_src_lang :: ComboBox -> IO String
get_src_lang cmb = do
    index <- comboBoxGetActive cmb
    return (langs !! index)

trans_click :: (EntryClass o, LabelClass o1) => o -> o1 -> ComboBox -> ComboBox -> IO ()
trans_click ent lbl cmb_src cmb_dest = do
    text <- get ent entryText
    src <- get_src_lang cmb_src
    dst <- get_dest_lang cmb_dest
    translated_text <- do_trans src dst text
    set lbl [ labelText := translated_text ]
