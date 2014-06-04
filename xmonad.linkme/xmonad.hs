import XMonad
import XMonad.Util.EZConfig
import XMonad.Config.Gnome

main = xmonad $ gnomeConfig { 
             modMask = mod4Mask,
             startupHook = do
                 startupHook gnomeConfig
                 myStartupHook
           , terminal = "urxvt" }
           `additionalKeysP`
           [ ("M-o", spawn "echo 'ALTERNATE_EDITOR=\"\" emacsclient -c & exit' | zsh")
           , ("M-f", spawn "firefox")
           , ("M-p", spawn "synapse") ]

myStartupHook :: X ()
myStartupHook = do
    spawn "~/scripts/startup.sh"

