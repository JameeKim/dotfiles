module XMonad.Util.Polybar
    ( polybarWsLogFile
    , action
    , font
    , underline
    , underlineColored
    , overline
    , overlineColored
    , reverseVideo
    , foreground
    , background
    )
where

import XMonad.Hooks.DynamicLog (wrap)

polybarWsLogFile :: String -> String
polybarWsLogFile = wrap "/tmp/.xmonad-" "-ws-log"

action :: String -> String -> String -> String
action cmd btn = wrap ("%{A" ++ btn ++ ":" ++ cmd ++ ":" ++ "}") "%{A}"

font :: Word -> String -> String
font ftNum = setAndReset "T" $ show ftNum

underlineColored :: String -> String -> String
underlineColored color = setAndReset "u" color . underline

underline :: String -> String
underline = wrap "%{+u}" "%{-u}"

overlineColored :: String -> String -> String
overlineColored color = setAndReset "o" color . overline

overline :: String -> String
overline = wrap "%{+o}" "%{-o}"

reverseVideo :: String -> String
reverseVideo = wrap "%{R}" "%{R}"

foreground :: String -> String -> String
foreground = setAndReset "F"

background :: String -> String -> String
background = setAndReset "B"

setAndReset :: String -> String -> String -> String
setAndReset char option =
    wrap ("%{" ++ char ++ option ++ "}") ("%{" ++ char ++ "-}")
