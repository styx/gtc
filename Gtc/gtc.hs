{-# LANGUAGE CPP #-}

module Main (
    main
) where

import Gt.Core
import Gt.Langs
import Control.Monad.Trans
import System.Console.Haskeline
import System.Environment
import System.Exit
import System.Posix.Terminal
import qualified Control.Exception.Extensible as E

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
    args <- getArgs
    case args of

        "-i":rest -> interactiveLoop rest
        ["-l"]    -> langs_list
        ["-h"]    -> usage

        "--interactive":rest -> interactiveLoop rest
        ["--list"]           -> langs_list
        ["--help"]           -> usage

        from:to:[]   -> interactiveLoop' from to
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
           historyFile = Just $ homedir ++ "/.gtc_history",
           autoAddHistory = True
           }

getHomeDir :: IO FilePath
getHomeDir =  getEnv "HOME" `E.catch` (const $ return "" :: E.SomeException -> IO String)

interactiveLoop' :: Lang -> Lang -> IO()
interactiveLoop' from to =
    getHomeDir >>= (\h -> runInputT (haskelineSettings h) loop)
    where
        promptLine :: IO String
        promptLine =
          do
            tty_type <- queryTerminal 0
            if tty_type
                then return "> "
                else return ""

        loop :: InputT IO()
        loop = do
            minput <- lift promptLine >>= getInputLine
            case minput of
                Nothing                -> return ()
                Just ""                -> loop
                -- reverse langs
                Just ('!':input)       -> translate to from input
                -- set new langs pair
                Just (':':input)       -> liftIO $ uncurry interactiveLoop' $ parse_cmd input
                -- normal mode
                Just input             -> translate from to input
            where
                parse_cmd :: String -> (Lang, Lang)
                parse_cmd input =
                    case wi of
                        nl1:nl2:[] -> (nl1, nl2)
                        _          -> (from, to)
                    where
                        wi = words input

                translate :: String -> String -> String -> InputT IO()
                translate l1 l2 i =
                    do t <- lift $ do_trans l1 l2 i
                       outputStrLn $ replicate 80 '-'
                       outputStrLn t
                       loop
