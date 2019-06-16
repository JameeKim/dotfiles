import XMonad
import XMonad.Actions.CycleWS
import XMonad.Actions.MyCommands
--import XMonad.Hooks.DebugKeyEvents
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.ServerMode
import XMonad.Layout.ToggleLayouts
import XMonad.Prompt
import XMonad.Prompt.Notify
import XMonad.Util.EZConfig (mkKeymap)
import XMonad.Util.NamedScratchpad
import XMonad.Util.Run (spawnPipe)
import XMonad.Util.WorkspaceCompare (getSortByIndex)

import Data.Monoid (All)
import System.Exit
import System.IO

import qualified Data.Map as M
import qualified XMonad.StackSet as W

-- | Set the Mod key to Super
myModMask :: KeyMask
myModMask = mod4Mask

-- | Set the terminal to rxvt-unicode
myTerminal :: String
myTerminal = "urxvt"

-- | Titles of the workspaces
workspaceTitles :: [String]
workspaceTitles =
    [ "\xf120" -- terminal
    , "\xf269" -- firefox
    , "\xf6d7" -- etc
    ]

-- | Keyboard bindings for workspaces
workspaceKeys :: [String]
workspaceKeys = flip (++) ["0", "-", "="] $ map show [1..9]

-- | Set workspaces titles
myWorkspaces :: [String]
myWorkspaces = (map mapToAction) . zip workspaceKeys $ workspaceTitles
    where
    mapToAction :: (String, String) -> String
    mapToAction (num, title) =
        xmobarAction
            ("xdotool key " ++ modToString myModMask ++  "+" ++ num)
            "123"
            (num ++ ":" ++ xmobarFont 1 title)

    modToString :: KeyMask -> String
    modToString m
        | m == mod1Mask = "alt"
        | otherwise = "super"

myScratchpads :: [NamedScratchpad]
myScratchpads =
    [ NS "terminal"
         "urxvt -name MainTerminal -title MainTerminal -n MainTerminal"
         (title =? "MainTerminal")
         (doRectFloat $ W.RationalRect 0 (1/2) 1 (1/2))
    ]

-- | Startup hook
myStartupHook :: X ()
myStartupHook = return ()

-- | Set window creation event handler
myManageHook :: ManageHook
myManageHook = namedScratchpadManageHook myScratchpads <+> composeAll
    [ className =? "MPlayer"     --> doFloat
    , className =? "stalonetray" --> doIgnore
    , className =? "Xmessage"    --> doFloat
    , className =? "Firefox"     --> doShift (myWorkspaces !! 1)
    , isDialog                   --> doCenterFloat
    ]

-- | Set window layouts
myLayoutHook = toggleLayouts Full $ Tall 1 (3/100) (1/2)

-- | Set the log hook for the status bar
myLogHook :: Handle -> X ()
myLogHook bar = do
    statusBarLog bar
    where
    -- get the log function from the pipe handle
    statusBarLog :: Handle -> X ()
    statusBarLog handle = dynamicLogWithPP def
        { ppOutput = hPutStrLn handle
        , ppCurrent = xmobarColor "cyan" "" . wrap "[" "]"
        , ppVisible = wrap "(" ")"
        , ppHidden = xmobarColor "white" "" . wrap " " " "
        , ppHiddenNoWindows = xmobarColor "grey" "" . wrap " " " "
        , ppVisibleNoWindows = Nothing
        , ppUrgent = xmobarColor "red" "cyan"
        , ppSep = wrap " " " " $ xmobarFont 1 "\xf48b"
        , ppWsSep = ""
        , ppTitle = xmobarColor "cyan" "" . shorten 40
        , ppTitleSanitize = xmobarRaw
        , ppOrder = id
        , ppSort = fmap (. namedScratchpadFilterOutWorkspace) getSortByIndex
        , ppExtras = []
        }

-- | Event hooks
myEventHook :: Event -> X All
myEventHook = serverModeEventHookCmd' myCommands

-- | Set custom keybindings
keys' :: XConfig Layout -> M.Map (ButtonMask, KeySym) (X ())
keys' conf@(XConfig {modMask = modm, terminal = term}) = mkKeymap conf $
    -- toggle status bar
    [ ("M-z", sendMessage ToggleStruts)

    -- stop or restart xmonad
    , ("M-q", spawn restart) -- reload xmonad
    , ("M-S-q", io $ exitWith ExitSuccess) -- exit xmonad

    -- kill current window
    , ("M-w", kill)

    -- move window focus
    , ("M-j", windows W.focusDown) -- move focus to next window
    , ("M-k", windows W.focusUp) -- move focus to previous window
    , ("M-m", windows W.focusMaster) -- move focus to master area

    -- move window
    , ("M-S-j", windows W.swapDown) -- move window to the next place
    , ("M-S-k", windows W.swapUp) -- move window to the previous place
    , ("M-S-m", windows W.swapMaster) -- move window to master area

    -- resize master window
    , ("M-S-h", sendMessage Shrink) -- decrease
    , ("M-S-l", sendMessage Expand) -- increase

    -- adjust the number of windows in the master area
    , ("M-,", sendMessage $ IncMasterN $ -1) -- decrease
    , ("M-.", sendMessage $ IncMasterN 1) -- increase

    -- spawn programs
    , ("M-<Return>", spawn term) -- default terminal
    , ("M-f", spawn "firefox") -- web browser
    , ("M-v", spawn $ term ++ " -name Vim -e vim") -- vim editor
    , ("M-S-v", spawn $ term ++ " - name Vim -e vim ~/.xmonad/xmonad.hs") -- edit xmonad config

    -- rofi
    , ("M-d", spawn "rofi -show drun -modi drun") -- open a desktop-like launcher
    , ("M-p", spawn "rofi -show run -modi run") -- open a shell prompt
    , ("M-<Tab>", spawn "rofi -show run -modi run,drun,ssh,window -sidebar-mode") -- main
    , ("M-S-/", spawn "rofi -theme Monokai -show keys -modi keys -width 80") -- show help for rofi

    -- scratchpad
    , ("<F12>", namedScratchpadAction myScratchpads "terminal") -- main terminal

    -- switch layouts
    , ("M-<Space>", sendMessage NextLayout) -- cycle through possible layouts
    , ("M-S-<Space>", setLayout $ layoutHook conf) -- reset the layouts on the current workspace
    , ("M-<Esc>", sendMessage $ Toggle "Full") -- toggle full layout mode
    , ("<F11>", sendMessage $ Toggle "Full") -- toggle full layout mode

    -- push a floating window back into tiling
    , ("M-t", withFocused $ windows . W.sink)

    -- cycle through the workspaces
    , ("M-l", avoidNSP nextWS) -- move to next workspace
    , ("M-h", avoidNSP prevWS) -- move to previous workspace
    ]
    ++
    -- switch workspaces or move window to another workspace
    [("M-" ++ shift ++ num, windows $ f ws)
        | (num, ws) <- zip workspaceKeys $ workspaces conf
        , (f, shift) <- [(W.view, ""), (W.shift, "S-")]]

    where
    -- string to restart xmonad
    restart :: String
    restart =
        "if type xmonad; then "
        ++ "xmonad --recompile && xmonad --restart; "
        ++ "else xmessage xmonad not in \\$PATH: \"$PATH\"; "
        ++ "fi"

    -- cycle through workspaces without getting into the named scratchpad ws
    avoidNSP :: X () ->  X ()
    avoidNSP move = do
        move
        ws <- withWindowSet $ pure . W.currentTag
        if ws == "NSP" then
            move
        else
            return ()

-- | Mouse bindings
mouse :: XConfig l -> M.Map (KeyMask, Button) (Window -> X ())
mouse conf@(XConfig {modMask = modm}) = M.fromList
    [ ((modm, button1), \w -> focus w >> mouseMoveWindow w >> windows W.shiftMaster)
    , ((modm, button2), windows . (W.shiftMaster .) . W.focusWindow)
    , ((modm, button3), \w -> focus w >> mouseResizeWindow w >> windows W.shiftMaster)
    ]

-- | Main configuration
main :: IO ()
main = do
    statusBar <- spawnPipe "xmobar ~/.config/xmobar/top"
    xmonad $ def
        { modMask = myModMask
        , terminal = myTerminal
        , workspaces = myWorkspaces
        , startupHook = docksStartupHook <+> myStartupHook
        , manageHook = manageDocks <+> myManageHook
        , layoutHook = avoidStruts myLayoutHook
        , logHook = myLogHook statusBar
        , handleEventHook = docksEventHook <+> myEventHook
        , keys = keys'
        , mouseBindings = mouse
        }

-- | Wrap the input text with the xmobar additional font tag
xmobarFont :: Int -> String -> String
xmobarFont idx input = "<fn=" ++ show idx ++ ">" ++ input ++ "</fn>"

-- vim:ts=4:shiftwidth=4:softtabstop=4:expandtab:
