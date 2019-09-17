{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

import XMonad
import XMonad.Actions.MyCommands
import XMonad.Config.Keymaps (myKeys, workspaceKeys)
import XMonad.Config.Workspaces (myWorkspaces)
import XMonad.Hooks.EwmhDesktops (ewmh)
import XMonad.Hooks.FadeInactive (fadeInactiveCurrentWSLogHook)
import XMonad.Hooks.ManageDocks (avoidStruts, docks)
import XMonad.Hooks.ManageHelpers
    (Side(..), doCenterFloat, doSideFloat, isDialog)
import XMonad.Hooks.MyLogHook (MyWsLogHookConfig(..), myWsLogHook)
import XMonad.Hooks.ServerMode
import XMonad.Layout.MyLayouts (myLayouts)
import XMonad.Util.Cursor (setDefaultCursor, xC_left_ptr)
import XMonad.Util.Polybar
    (action, font, foreground, underlineColored, polybarWsLogFile)
import XMonad.Util.Run (safeSpawn)

import Data.Monoid (All)
import System.Environment (getEnv)

import qualified Data.Map as M
import qualified XMonad.StackSet as W

-- | Set the Mod key to Super
myModMask :: KeyMask
myModMask = mod4Mask

-- | Set the terminal to rxvt-unicode
myTerminal :: String
myTerminal = "termite"

-- | Startup hook
myStartupHook :: X ()
myStartupHook = setDefaultCursor xC_left_ptr

-- | Set window creation event handler
myManageHook :: ManageHook
myManageHook = composeAll
    [ className =? "MPlayer" --> doFloat
    , className =? "Xmessage" --> doFloat
    , className =? "TelegramDesktop" --> doShift "chat"
    , className =? "discord" --> doShift "chat"
    , className =? "Evolution" --> doShift "chat"
    , className =? "code-oss" --> doShift "vscode"
    , className =? "R_x11" --> doSideFloat SE -- R script graphics window
    , isDialog --> doCenterFloat
    ]

-- | Set the log hook
myLogHook :: String -> X ()
myLogHook user = do
    fadeInactiveCurrentWSLogHook 0.9
    myWsLogHook WsLogHookConfig
        { wsCurrentFmt = (.) <$> underlineColored <*> foreground $ "#00ffff"
        , wsVisibleFmt      = foreground "#80ffff"
        , wsVisibleNoWinFmt = foreground "#80ffff"
        , wsHiddenFmt       = foreground "#ffffff"
        , wsHiddenNoWinFmt  = foreground "#cccccc"
        , wsUrgentFmt       = foreground "#e84f4f"
        , wsListFmt         =
            action "xmonad-client view_left" "4"
            . action "xmonad-client view_right" "5"
            . action "xmonad-client view_up"    "8"
        , wsListItemFmt     = \i -> action (moveToWsCmd i) "1"
            . action (moveToWsCmd i ++ "; xmonad-client view_down") "6"
        , wsIconFmt         = font 2
        , wsShowNumber      = True
        , wsNumberFmt       = font 4
        , wsLogOutFile      = polybarWsLogFile user
        , wsLogSep          = " - "
        , wsShowLevel       = True
        , wsLevelFmt        = action "xmonad-client view_up" "1" . font 4
        , wsShowLayout      = True
        , wsLayoutFmt       =
            action "xmonad-client layout_next" "1"
            . action "xmonad-client layout_full"  "3"
            . action "xmonad-client layout_reset" "2"
            . font 4
        }
  where
    moveToWsCmd :: Int -> String
    moveToWsCmd i = "xmonad-client view" ++ workspaceKeys !! i

-- | Event hooks
myEventHook :: Event -> X All
myEventHook = serverModeEventHookCmd' myCommands

-- | Mouse bindings
mouse :: XConfig l -> M.Map (KeyMask, Button) (Window -> X ())
mouse _conf@XConfig { modMask = modm } = M.fromList
    [ ( (modm, button1)
      , \w -> focus w >> mouseMoveWindow w >> windows W.shiftMaster
      )
    , ((modm, button2), windows . (W.shiftMaster .) . W.focusWindow)
    , ( (modm, button3)
      , \w -> focus w >> mouseResizeWindow w >> windows W.shiftMaster
      )
    ]

-- | Main configuration
main :: IO ()
main = do
    user <- getEnv "USER"
    safeSpawn "mkfifo" [polybarWsLogFile user]
    spawn "polybar_start"
    xmonad . ewmh . docks $ def
        { modMask            = myModMask
        , terminal           = myTerminal
        , workspaces         = myWorkspaces
        , startupHook        = myStartupHook
        , manageHook         = myManageHook
        , layoutHook         = avoidStruts myLayouts
        , logHook            = myLogHook user
        , handleEventHook    = myEventHook
        , keys               = myKeys
        , mouseBindings      = mouse
        , focusFollowsMouse  = False
        , clickJustFocuses   = False
        , borderWidth        = 0
        , normalBorderColor  = "#222222"
        , focusedBorderColor = "#f0544c"
        }

-- vim:ts=4:sw=4:sts=4:et:
