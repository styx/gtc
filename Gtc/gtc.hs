
module Main (
    main
) where

import Gt.Core
import Gt.Langs
import Data.List (intercalate)
import System.Exit
import qualified System.Environment.UTF8 as U

usage :: IO ()
usage =
  do
     mapM_ putStrLn [ "    Usage: 'fromLang' 'toLang' 'text'"
                    , "    Lang is 2 sumbols code, i.e. be, en, ru.\n"
                    , "        --help or -h - to see this help."
                    , "        --list or -l - list of available languages.\n"
                    ]
     exitWith $ ExitFailure 1

langs_list :: IO ()
langs_list = putStrLn $ "\nList of available languages:\n\n" ++
             (concat $ zipWith (\l d -> "    " ++ l ++ " - " ++ d ++ "\n") langs langs_descrs)

main :: IO ()
main =
  do
     -- TODO: Need to detecet terminal encoding and make convertions
     args <- U.getArgs
     case args of

        ["-l"]       -> langs_list
        ["-h"]       -> usage

        ["--list"]   -> langs_list
        ["--help"]   -> usage

        from:to:rest -> do_trans from to (intercalate " " rest) >>= putStrLn
        _            -> usage
     exitWith ExitSuccess
