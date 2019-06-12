import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Util.Run(spawnPipe)
import System.IO

myModMask :: KeyMask
myModMask = mod4Mask

myTerminal :: String
myTerminal = "urxvt"

myWorkspaces :: [String]
myWorkspaces = (map mapToAction) . zip [1..] $ ["dev", "web"] ++ map show [3..9]
    where
        mapToAction (num, title) =
            "<action=xdotool key super+"
                ++ show num
                ++ ">"
                ++ xmobarEscape title
                ++ "</action>"

myManageHook :: ManageHook
myManageHook = manageDocks <+> manageHookPerApp

manageHookPerApp :: ManageHook
manageHookPerApp = composeAll
    [ className =? "MPlayer" --> doFloat
    , className =? "stalonetray" --> doIgnore
    ]

myLogHook :: Handle -> X ()
myLogHook handle = dynamicLogWithPP xmobarPP
    { ppOutput = hPutStrLn handle
    , ppCurrent = xmobarColor "yellow" "" . wrap "[" "]"
    , ppHiddenNoWindows = xmobarColor "grey" ""
    , ppTitle = xmobarColor "green" "" . shorten 40
    , ppVisible = wrap "(" ")"
    , ppUrgent = xmobarColor "red" "yellow"
    }

main :: IO ()
main = do
    statusBar <- spawnPipe "xmobar"
    xmonad $ def
        { modMask = myModMask
        , terminal = myTerminal
        , workspaces = myWorkspaces
        , manageHook = myManageHook
        , logHook = myLogHook statusBar
        }

xmobarEscape :: String -> String
xmobarEscape = concatMap doubleLts
    where
        doubleLts '<' = "<<"
        doubleLts x = [x]
