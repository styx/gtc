
module Main (
    main
) where

import Gt.Core
import Gt.Langs
import Control.Monad.Trans
import System.Console.Haskeline hiding (catch)
import System.Environment
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
interactiveLoop :: [String] -> IO()
interactiveLoop params =
    case params of
        from:to:_ -> interactiveLoop' from to
        _         -> usage

haskelineSettings :: String -> Settings IO
haskelineSettings homedir = Settings {
           complete = noCompletion,
           historyFile = Just $ homedir ++ ".gtc_history",
           autoAddHistory = True
           }

getHomeDir :: IO FilePath
getHomeDir = catch (getEnv "HOME") (\_ -> return "")

interactiveLoop' :: Lang -> Lang -> IO()
interactiveLoop' from to =
  do
    h <- getHomeDir
    runInputT (haskelineSettings h) loop
        where
            loop :: InputT IO()
            loop = do
                minput <- getInputLine "> "
                case minput of
                    Nothing -> return ()
                    Just "" -> loop
                    Just input -> do
                                     t <- lift $ do_trans from to input
                                     outputStrLn t
                                     loop
