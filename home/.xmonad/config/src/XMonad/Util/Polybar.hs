module XMonad.Util.Polybar
    ( polybarWsLogFile
    , polybarWsPP
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

import XMonad (io)
import XMonad.Config.Workspaces (wsFilterWsLevel)
import XMonad.Hooks.DynamicLog (PP(..), wrap)
import XMonad.Util.WorkspaceCompare (getSortByIndex)

import Control.Applicative (liftA2)

polybarWsLogFile :: String -> String
polybarWsLogFile = wrap "/tmp/.xmonad-" "-ws-log"

polybarWsPP :: String -> PP
polybarWsPP user = PP
    { ppCurrent          = (.) <$> underlineColored <*> foreground $ "#ff0000"
    , ppVisible          = id
    , ppHidden           = foreground "#ffffff"
    , ppHiddenNoWindows  = id
    , ppVisibleNoWindows = Nothing
    , ppUrgent           = id
    , ppSep              = " : "
    , ppWsSep            = " "
    , ppTitle            = const ""
    , ppTitleSanitize    = id
    , ppLayout           = id
    , ppOrder            = \l -> [head l]
    , ppOutput           = io . appendFile (polybarWsLogFile user) . (++ "\n")
    , ppSort             = liftA2 (.) getSortByIndex wsFilterWsLevel
    , ppExtras           = []
    }

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
