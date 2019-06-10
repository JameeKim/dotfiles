import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Util.Run(spawnPipe)
import System.IO

myModMask = mod4Mask
myTerminal = "urxvt"

main = do
    xmproc <- spawnPipe "xmobar"
    xmonad $ defaultConfig
        { modMask = myModMask
        , terminal = myTerminal
        , logHook = dynamicLogWithPP xmobarPP
                        { ppOutput = hPutStrLn xmproc
                        , ppTitle = xmobarColor "green" "" . shorten 50
                        }
        }
