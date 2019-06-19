module XMonad.Util.Rofi ( rofiMessage
                        , rofiMessageTheme
                        , cmdEscape
                        ) where

import XMonad ( X, spawn )
import XMonad.Hooks.DynamicLog ( wrap )

-- | Show a rofi message
rofiMessage :: String -> X ()
rofiMessage = rofiMessageTheme "Monokai"

-- | Show a rofi message, with a theme
rofiMessageTheme :: String -> String -> X ()
rofiMessageTheme theme msg = spawn $ "rofi -theme " ++ cmdEscape theme ++ " -e " ++ cmdEscape msg

-- | Escape spaces to use in the command line
cmdEscape :: String -> String
cmdEscape = wrap "\"" "\"" . concat . map doEscape
    where
    doEscape :: Char -> String
    doEscape '"' = "\\\""
    doEscape c = [c]

-- vim:ts=4:shiftwidth=4:softtabstop=4:expandtab:
