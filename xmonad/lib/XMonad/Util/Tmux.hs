module XMonad.Util.Tmux ( spawnNewTmuxSession
                        , attachTmuxSession
                        , attachOrCreateTmuxSession
                        ) where

import XMonad ( X )
import XMonad.Util.Dmenu ( menuArgs, menuMapArgs )
import XMonad.Util.Rofi ( cmdEscape, rofiMessage )
import XMonad.Util.Run ( runInTerm, runProcessWithInput )

import Data.Map ( fromList )

-- | Create a new session and attach to it
spawnNewTmuxSession :: X ()
spawnNewTmuxSession = do
    sName <- menuArgs
        "dmenu"
        ["-p", "New session name: "]
        []
    runInTerm "" $ tmuxCreateNewSessionCmd sName
                ++ " && "
                ++ tmuxAttachToSessionCmd sName

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
    sName <- menuMapArgs
        "dmenu"
        ["-p", "Select session to attach to: "]
        (fromList . map (\l -> (l, l)) $ lsList)
    case sName of
        Just s -> runInTerm "" $
            tmuxAttachToSessionCmd $ takeWhile (/= ':') s
        -- TODO: do not show this when cancelled
        Nothing -> rofiMessage "Wrong session name given!"

-- | Attach to an existing session or create a new one
attachOrCreateTmuxSession :: X ()
attachOrCreateTmuxSession = do
    lsList <- tmuxGetRunningSessions
    if length lsList > 0
    then attachTmuxSession' lsList
    else spawnNewTmuxSession

-- | Command to create a new session with the given name
tmuxCreateNewSessionCmd :: String -> String
tmuxCreateNewSessionCmd name = "tmux new -s " ++ cmdEscape name

-- | Command to attach to the given session
tmuxAttachToSessionCmd :: String -> String
tmuxAttachToSessionCmd name = "tmux attach -t " ++ cmdEscape name

-- | Get a list of currently running sessions
tmuxGetRunningSessions :: X [String]
tmuxGetRunningSessions = lines <$> runProcessWithInput "tmux" ["ls"] ""

-- vim:ts=4:shiftwidth=4:softtabstop=4:expandtab:
