
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
    window          <- xmlGetWidget xml castToWindow "gtg_main"
    lbl_translation <- xmlGetWidget xml castToLabel  "lbl_translation"
    ent_trans_text  <- xmlGetWidget xml castToEntry  "ent_trans_text"
    btn_translate   <- xmlGetWidget xml castToButton "btn_translate"
    btn_close       <- xmlGetWidget xml castToButton "btn_close"

    -- Actions
    onClicked       btn_close      (widgetDestroy window)
    onClicked       btn_translate  (trans_click ent_trans_text lbl_translation)
    onEntryActivate ent_trans_text (trans_click ent_trans_text lbl_translation)
    onDestroy       window         mainQuit

    -- Let's go
    widgetShowAll window
    mainGUI

-- trans_click ::
trans_click ent lbl = do
                        text <- get ent entryText
                        translated_text <- do_trans "en" "ru" text
                        set lbl [ labelText := (decodeString translated_text) ]
                        U.putStrLn $ decodeString $ translated_text
