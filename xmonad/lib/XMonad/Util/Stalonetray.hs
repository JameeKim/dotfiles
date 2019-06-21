module XMonad.Util.Stalonetray ( trayKillAndSpawn ) where

import XMonad ( spawn )
import XMonad.Core ( catchIO )

import System.Environment ( lookupEnv )
import System.Process ( callProcess )

trayKillAndSpawn :: Int -> IO ()
trayKillAndSpawn sec = do
    maybeUser <- lookupEnv "USER"
    let user = case maybeUser of
            Nothing -> "jamee"
            Just u -> u
    catchIO $ callProcess "killall" ["-u", user, "stalonetray"]
    spawn $ "sleep " ++ show sec ++ " && stalonetray"

-- vim:ts=4:shiftwidth=4:softtabstop=4:expandtab:
