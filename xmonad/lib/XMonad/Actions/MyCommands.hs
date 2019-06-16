module XMonad.Actions.MyCommands ( myCommands ) where

import XMonad
import XMonad.Actions.Commands
import XMonad.Prompt.Notify

import qualified XMonad.StackSet as W

myWsCommands :: X [(String, X ())]
myWsCommands = asks (workspaces . config) >>= cmdFromWs
    where
    cmdFromWs :: [String] -> X [(String, X ())]
    cmdFromWs ws = return
        [(cmd ++ idx, windows $ f wsName)
            | (wsName, idx) <- zip ws $ flip (++) ["0", "-", "="] $ map show [1..9]
            , (f, cmd) <- [(W.view, "view"), (W.shift, "shift")]]

myCommands :: X [(String, X ())]
myCommands = do
    wsCmds <- myWsCommands
    let res = wsCmds ++ otherCmds
    return (mkListCmd res:res)
    where
    otherCmds :: [(String, X ())]
    otherCmds =
        [ ("terminal", asks (terminal . config) >>= spawn)
        , ("__sys_info", sysInfoPrompt def)
        ]

    mkListCmd :: [(String, X ())] -> (String, X ())
    mkListCmd cmds =
        ("list", runCommand cmds)

-- vim:ts=4:shiftwidth=4:softtabstop=4:expandtab:
