module XMonad.Prompt.Notify ( NotifyPrompt(..)
                            , mkNotifyPrompt
                            , sysInfoPrompt
                            ) where

import XMonad (X)
import XMonad.Prompt ( XPConfig
                     , XPrompt
                     , mkComplFunFromList
                     , mkXPrompt
                     , showXPrompt
                     )

data NotifyPrompt = NotifyPrompt String
instance XPrompt NotifyPrompt where
    showXPrompt (NotifyPrompt s) = s ++ "\n\tOK (ENTER)"

mkNotifyPrompt :: String -> XPConfig -> X()
mkNotifyPrompt s c = mkXPrompt (NotifyPrompt s) c (mkComplFunFromList []) $ \_ -> return ()

sysInfoPrompt :: XPConfig -> X()
sysInfoPrompt = mkNotifyPrompt
    "Made with Haskell \xe61f"

-- vim:ts=4:shiftwidth=4:softtabstop=4:expandtab:
