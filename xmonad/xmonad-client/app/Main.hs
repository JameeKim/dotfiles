module Main ( main ) where

import Data.Char
import Graphics.X11.Xlib
import Graphics.X11.Xlib.Extras
import System.Environment
import System.IO

-- | Main function
main :: IO ()
main = parse True "XMONAD_COMMAND" =<< getArgs

-- | Parse the args
parse :: Bool -> String -> [String] -> IO ()
parse input addr args = case args of
    ["--"]
        -- get commands from stdin
        | input -> repl addr
        -- finish
        | otherwise -> return ()

    -- send all commands
    ("--":xs) -> sendAll addr xs

    -- parse the rest of the args with the given atom name
    ("-a":a:xs) -> parse input a xs

    -- show help
    ("-h":_) -> showHelp

    -- show help
    ("--help":_) -> showHelp

    -- show help
    ("-?":_) -> showHelp

    -- unknown option error
    (a@('-':_):_) -> hPutStrLn stderr $ "Unknown option: " ++ a

    -- send the command, then parse the rest
    (x:xs) -> sendCommand addr x >> parse False addr xs

    -- empty args
    []
        -- get commands from stdin
        | input -> repl addr
        -- finish
        | otherwise -> return ()

-- | Get commands from stdin and send
repl :: String -> IO ()
repl addr = do
    -- check if the input is eof
    e <- isEOF
    case e of
        -- finish
        True -> return ()
        -- send the command and wait for another input
        False -> do
            l <- getLine
            sendCommand addr l
            repl addr

-- | Send all of the commands
sendAll :: String -> [String] -> IO ()
sendAll addr ss = foldr (\a b -> sendCommand addr a >> b) (return ()) ss

-- | Send the single command with the specified atom name
sendCommand :: String -> String -> IO ()
sendCommand addr s = do
    d <- openDisplay ""
    rw <- rootWindow d $ defaultScreen d
    a <- internAtom d addr False
    m <- internAtom d s False
    -- allocate memory for the event, set the data, and send it
    allocaXEvent $ \e -> do
        setEventType e clientMessage
        setClientMessageEvent e rw a 32 m currentTime
        sendEvent d rw False structureNotifyMask e
        sync d False

-- | Print out the help message
showHelp :: IO ()
showHelp = do
    pn <- getProgName
    putStrLn $ "Send commands to a running instance of xmonad.\n"
            ++ "xmonad.hs must be configured with XMonad.Hooks.ServerMode\n"
            ++ "to work.\n"
            ++ "-a\tatomname can be used at any point in the command line\n"
            ++ "\targuments to change which atom it is sending on.\n"
            ++ "\tIf sent with no arguments or only -a atom arguments,\n"
            ++ "\tit will read commands from stdin.\n"
            ++ "\tDefaults to XMONAD_COMMAND.\n"
            ++ "Ex:\n"
            ++ "\t" ++ pn ++ " cmd1 cmd2\n"
            ++ "\t" ++ pn ++ " -a XMONAD_COMMAND cmd1 cmd2 cmd3\n"
            ++ "\t\t-a XMONAD_PRINT hello world\n"
            ++ "\t" ++ pn ++ " -a XMONAD_PRINT # will read data from stdin\n"

-- vim:ts=4:shiftwidth=4:softtabstop=4:expandtab:
