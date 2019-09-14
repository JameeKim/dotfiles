module XMonad.Actions.MyCommands
    ( myCommands
    )
where

import XMonad
import XMonad.Actions.Commands
import XMonad.Config.Keymaps (workspaceKeys, moveOrShiftWS)
import XMonad.Config.Workspaces (Direction2D(..), wsMove, wsShift)
import XMonad.Layout.ToggleLayouts (ToggleLayout(..))
import XMonad.Prompt.Notify
import XMonad.Util.Tmux (spawnNewTmuxSession, attachOrCreateTmuxSession)

import qualified XMonad.StackSet as W

myWsCommands :: X [(String, X ())]
myWsCommands =
    return
        $  [ (cmd ++ "_" ++ dirTxt, op direction)
           | (cmd, op) <- [("view", wsMove), ("shift", wsShift)]
           , (direction, dirTxt) <-
               [(U, "up"), (D, "down"), (L, "left"), (R, "right")]
           ]
        ++ [ (cmd ++ key, moveOrShiftWS f num)
           | (key, num) <- zip workspaceKeys [0 ..]
           , (f  , cmd) <- [(W.view, "view"), (W.shift, "shift")]
           ]

myCommands :: X [(String, X ())]
myCommands = do
    wsCmds <- myWsCommands
    let res = wsCmds ++ otherCmds
    return (mkListCmd res : res)
  where
    otherCmds :: [(String, X ())]
    otherCmds =
        [ ("layout_next" , sendMessage NextLayout)
        , ("layout_full" , sendMessage $ Toggle "Full")
        , ("layout_reset", asks (layoutHook . config) >>= setLayout)
        , ("terminal"    , attachOrCreateTmuxSession)
        , ("new_terminal", spawnNewTmuxSession)
        , ("__sys_info"  , sysInfoPrompt def)
        ]

    mkListCmd :: [(String, X ())] -> (String, X ())
    mkListCmd cmds = ("list", runCommand cmds)

-- vim:ts=4:sw=4:sts=4:et:
