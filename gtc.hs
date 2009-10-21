
import Data.List
import Data.Maybe
import Network.URL
import Network.HTTP
import Control.Monad
import System.Exit
import System.Environment
import qualified System.Environment.UTF8 as U

translation_url :: String -> String -> String -> String

translation_url sl tl text =
    exportURL $ foldl add_param base_url [("text", text), ("sl", sl), ("tl", tl)]
        where base_url = fromJust $ importURL "http://translate.google.com/translate_a/t?client=t&pc=0&oc=1&hl=en&ie=UTF-8&oe=UTF-8"

-- TODO: Need to add JSON parsing
do_translation :: String -> String -> String -> IO String
do_translation sl tl word =
  do result <- simpleHTTP (getRequest $ translation_url sl tl word) >>= getResponseBody
     putStrLn $ translation_url "en" "ru" word
     return result

usage :: IO ()
usage =
  do
     putStrLn "Usage: 'fromLang' 'toLang' 'text'"
     putStrLn "Lang is 2 sumbols code, i.e. by, en, ru."
     exitWith $ ExitFailure 1

main :: IO ()
main =
  do
     -- TODO: Need to detecet terminal encoding and make convertions
     args <- U.getArgs
     case args of
        from:to:rest -> do_translation from to (intercalate " " rest) >>= putStrLn
        _        -> usage
     exitWith $ ExitSuccess
