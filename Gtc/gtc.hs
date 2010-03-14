
module Main (
    main
) where

import Gt.Core
import Gt.Langs
import Control.Monad
import System.IO.Error
import System.Exit
import qualified System.Environment.UTF8 as U

usage :: IO()
usage =
  do
     mapM_ putStrLn [ "    Usage:"
                    , "         Standart mode: gtc 'fromLang' 'toLang' 'text'"
                    , "         Interactive mode: gtc -i 'fromLang' 'toLang'"
                    , "             Where 'Lang' is 2 sumbols code, i.e. be, en, ru.\n"
                    , "    Commandline keys:"
                    , "        --interactive or -i - interactive mode."
                    , "        --help        or -h - to see this help."
                    , "        --list        or -l - list of available languages.\n"
                    ]
     exitWith $ ExitFailure 1

langs_list :: IO()
langs_list = putStrLn $ "\nList of available languages:\n\n" ++
             concat (zipWith (\l d -> "    " ++ l ++ " - " ++ d ++ "\n") langs langs_descrs)

main :: IO()
main =
  do
    -- TODO: Need to detecet terminal encoding and make convertions
    args <- U.getArgs
    case args of

        "-i":rest -> interactiveLoop rest
        ["-l"]    -> langs_list
        ["-h"]    -> usage

        "--interactive":rest -> interactiveLoop rest
        ["--list"]           -> langs_list
        ["--help"]           -> usage


        -- Old variant with 'single line' processing
        -- from:to:rest -> do_trans from to (intercalate " " rest) >>= putStrLn

        from:to:rest -> mapM_ ((>>= putStrLn) . do_trans from to) rest
        _            -> usage

    exitWith ExitSuccess

-- Interactive mode processor
-- TODO:
-- 1) It shoud be possible to memoize: interactiveLoop' from to
-- 2) Need to remove explicit recursion
-- 3) Need to skip blank lines (including lines consist of white spaces only)
-- 4) Split results with '-------------------' ???
-- 5) Remove some redunt carriage returns to make results looks prettie
interactiveLoop :: [String] -> IO()
interactiveLoop params =
    case params of
        from:to:_ -> interactiveLoop' from to
        _         -> usage

interactiveLoop' :: Lang -> Lang -> IO()
interactiveLoop' from to =
  do
    maybeLine <- getLine'
    case maybeLine of
        Nothing   -> return ()
        Just line -> do
                       do_trans from to line >>= putStrLn
                       interactiveLoop' from to

getLine' :: IO(Maybe String)
getLine' =
    liftM Just getLine `catch` eofHandler
    where eofHandler e = if isEOFError e
            then return Nothing
            else ioError e
