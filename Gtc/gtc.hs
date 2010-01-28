
module Main (
    main
) where

import Gt.Core
import Data.List (intercalate)
import System.Exit
import qualified System.Environment.UTF8 as U

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
        from:to:rest -> do_trans from to (intercalate " " rest) >>= putStrLn
        _            -> usage
     exitWith ExitSuccess
