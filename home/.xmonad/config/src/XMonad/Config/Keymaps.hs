module XMonad.Config.Keymaps
    ( myKeys
    , workspaceKeys
    , moveOrShiftWS
    )
where

import XMonad
import XMonad.Actions.CycleWS (prevWS, nextWS)
import XMonad.Config.Workspaces
    ( wsGetWsInCurrentLevel
    , wsMoveToParent
    , wsMoveToChild
    , wsMovePrev
    , wsMoveNext
    , wsShiftToParent
    , wsShiftToChild
    , wsShiftToNext
    , wsShiftToPrev
    )
import XMonad.Hooks.ManageDocks (ToggleStruts(..))
import XMonad.Layout.ToggleLayouts (ToggleLayout(..))
import XMonad.Util.EZConfig (mkKeymap)
import XMonad.Util.Tmux (attachOrCreateTmuxSession, spawnNewTmuxSession)

import Control.Monad (when)
import System.Exit (exitSuccess)

import qualified Data.Map as M
import qualified XMonad.StackSet as W

-- | Customized key bindings
myKeys :: XConfig Layout -> M.Map (ButtonMask, KeySym) (X ())
myKeys conf@XConfig { terminal = term } =
    mkKeymap conf
        $  [
            -- toggle status bar
             ( "M-z"
             , sendMessage ToggleStruts
             )

            -- stop or restart xmonad
           , ( "M-q"
             , spawn restartCmd
             ) -- reload xmonad
           , ( "M-S-q"
             , io exitSuccess
             ) -- exit xmonad

            -- kill current window
           , ( "M-w"
             , kill
             )

            -- move through workspace tree
           , ( "M-k"
             , wsMoveToParent
             ) -- move to parent node
           , ( "M-j"
             , wsMoveToChild
             ) -- move to the first child
           , ( "M-h"
             , wsMovePrev
             ) -- move to the previous sibling
           , ( "M-l"
             , wsMoveNext
             ) -- move to the next sibling

            -- shift window to another workspace
           , ( "M-S-k"
             , wsShiftToParent
             ) -- shift to parent node
           , ( "M-S-j"
             , wsShiftToChild
             ) -- shift to the first child
           , ( "M-S-h"
             , wsShiftToPrev
             ) -- shift to the previous sibling
           , ( "M-S-l"
             , wsShiftToNext
             ) -- shift to the next sibling

            -- fallback method to move workspaces
           , ( "M-<Left>"
             , prevWS
             ) -- move to previous workspace
           , ( "M-<Right>"
             , nextWS
             ) -- move to next workspace
           , ( "M-S-<Left>"
             , prevWS
             ) -- shift to previous workspace
           , ( "M-S-<Right>"
             , nextWS
             ) -- shift to next workspace

            -- move window focus
           , ( "M-n"
             , windows W.focusDown
             ) -- move focus to next window
           , ( "M-p"
             , windows W.focusUp
             ) -- move focus to previous window
           , ( "M-m"
             , windows W.focusMaster
             ) -- move focus to master area

            -- move window
           , ( "M-S-n"
             , windows W.swapDown
             ) -- move window to the next place
           , ( "M-S-p"
             , windows W.swapUp
             ) -- move window to the previous place
           , ( "M-S-m"
             , windows W.swapMaster
             ) -- move window to master area

            -- resize master window
           , ( "M-C-h"
             , sendMessage Shrink
             ) -- decrease
           , ( "M-C-l"
             , sendMessage Expand
             ) -- increase

            -- adjust the number of windows in the master window area
           , ( "M-,"
             , sendMessage . IncMasterN $ -1
             ) -- decrease
           , ( "M-."
             , sendMessage . IncMasterN $ 1
             ) -- increase

            -- switch layouts
           , ( "M-<Space>"
             , sendMessage NextLayout
             ) -- cycle through possible layouts
           , ( "M-S-<Space>"
             , setLayout $ layoutHook conf
             ) -- reset the layouts on the current workspace
           , ( "M-<Esc>"
             , sendMessage $ Toggle "Full"
             ) -- toggle full layout mode
           , ( "<F11>"
             , sendMessage $ Toggle "Full"
             ) -- toggle full layout mode

            -- push a floating window back into tiling
           , ( "M-t"
             , withFocused $ windows . W.sink
             )

            -- spawn programs
           , ( "M-<Return>"
             , spawn $ term ++ " --title Terminal"
             ) -- default terminal
           , ( "M-'"
             , attachOrCreateTmuxSession
             ) -- attach to a tmux session or create one if none exist
           , ( "M-S-'"
             , spawnNewTmuxSession
             ) -- create a new tmux session
           , ( "M-f"
             , spawn "firefox"
             ) -- web browser
           , ( "M-v"
             , spawn $ term ++ " --title Nvim -e nvim"
             ) -- vim editor

            -- rofi
           , ( "M-d"
             , spawn "rofi -show drun -modi drun"
             ) -- open a desktop-like launcher
           , ( "M-<Tab>"
             , spawn "rofi -show run -modi run"
             ) -- open a shell prompt
           , ( "M-/"
             , spawn "rofi -show window -modi window"
             ) -- window list
           , ( "M-`"
             , spawn "rofi -show run -modi run,drun,ssh,window -sidebar-mode"
             ) -- main
           , ( "M-S-/"
             , spawn "rofi -show keys -modi keys -width 80"
             ) -- show help for rofi

            -- multimedia keys
            -- TODO test on a real computer
           , ( "<XF86AudioRaiseVolume>"
             , spawn "amixer -q sset Master 5%+"
             ) -- volume up
           , ( "<XF86AudioLowerVolume>"
             , spawn "amixer -q sset Master 5%-"
             ) -- volume down
           , ( "<XF86AudioMute>"
             , spawn "amixer -q sset Master toggle"
             ) -- toggle volume mute and unmute
           , ( "<XF86MonBrightnessUp>"
             , sendMessage NextLayout
             ) -- brightness up
           , ("<XF86MonBrightnessDown>", sendMessage NextLayout) -- brightness down
           ]
        ++
            -- workspace operations by number keys
           [ ("M-" ++ shift ++ key, moveOrShiftWS f num)
           | (key, num) <- zip workspaceKeys [0 ..]
           , (f, shift) <- [(W.view, ""), (W.shift, "S-")]
           ]
  where
    -- string to restart xmonad
    restartCmd :: String
    restartCmd =
        "if type xmonad; then "
            ++ "xmonad --recompile && xmonad --restart; "
            ++ "else xmessage xmonad not in \\$PATH: \"$PATH\"; "
            ++ "fi"

workspaceKeys :: [String]
workspaceKeys = ["1", "2", "3", "4", "5", "6", "7", "8", "9", "0", "-", "="]

moveOrShiftWS :: (WorkspaceId -> WindowSet -> WindowSet) -> Int -> X ()
moveOrShiftWS op idx = do
    wsList <- wsGetWsInCurrentLevel
    when (idx < length wsList) $ windows $ op (fst $ wsList !! idx)
