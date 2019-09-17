module XMonad.Util.Tmux
    ( spawnNewTmuxSession
    , attachTmuxSession
    , attachOrCreateTmuxSession
    )
where

import XMonad (X)
import XMonad.Hooks.DynamicLog (wrap)
import XMonad.Util.Dmenu (menuArgs, menuMapArgs)
import XMonad.Util.Rofi (cmdEscape, rofiMessage)
import XMonad.Util.Run (runInTerm, runProcessWithInput)

import Data.Map (fromList)

-- | Create a new session and attach to it
spawnNewTmuxSession :: X ()
spawnNewTmuxSession = do
    sName <- menuArgs "dmenu" ["-p", "New session name: "] []
    if null sName
        then return ()
        else runInTerm "--title Tmux" . wrap "\"" "\"" $ tmuxCreateNewSessionCmd
            sName

-- | Attach to the selected session after checking if any exist
attachTmuxSession :: X ()
attachTmuxSession = do
    sessions <- tmuxGetRunningSessions
    if null sessions
        then rofiMessage "No runing sessions currently exist!"
        else attachTmuxSession' sessions

-- | Attach to the selected session, not checking if any exist
attachTmuxSession' :: [String] -> X ()
attachTmuxSession' lsList = do
    sName <- menuArgs "dmenu" ["-p", "Select session to attach to: "] lsList
    if null sName
        then return ()
        else if sName `elem` lsList
            then
                runInTerm "--title Tmux"
                . wrap "'" "'"
                . tmuxAttachToSessionCmd
                . takeWhile (/= ':')
                $ sName
            else rofiMessage "Wrong session name given!"

-- | Attach to an existing session or create a new one
attachOrCreateTmuxSession :: X ()
attachOrCreateTmuxSession = do
    lsList <- tmuxGetRunningSessions
    if not (null lsList) then attachTmuxSession' lsList else spawnNewTmuxSession

-- | Command to create a new session with the given name
tmuxCreateNewSessionCmd :: String -> String
tmuxCreateNewSessionCmd name = "tmux new -s " ++ cmdEscape name

-- | Command to attach to the given session
tmuxAttachToSessionCmd :: String -> String
tmuxAttachToSessionCmd name = "tmux attach -t " ++ cmdEscape name

-- | Get a list of currently running sessions
tmuxGetRunningSessions :: X [String]
tmuxGetRunningSessions = lines <$> runProcessWithInput
    "tmux"
    [ "ls"
    , "-F"
    , "#S:#{session_id} - #{session_windows} windows"
        ++ " - #{session_attached} attached#{?session_alerts, (#{session_alerts}),}"
    ]
    ""

-- vim:ts=4:shiftwidth=4:softtabstop=4:expandtab:
