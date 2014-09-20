import           XMonad
import           XMonad.Config.Gnome
import           XMonad.Util.EZConfig

main = xmonad $ gnomeConfig {
             modMask = mod4Mask,
             startupHook = do
                 startupHook gnomeConfig
                 myStartupHook
           , terminal = "urxvt" }
           `additionalKeysP`
           [ ("M-o", spawn "echo 'ALTERNATE_EDITOR=\"\" emacsclient -c & exit' | zsh")
           , ("M-f", spawn "firefox")
           , ("M-m", spawn "banshee")
           , ("M-F1", spawn "synclient TouchPadOff=1")
           , ("M-F2", spawn "synclient TouchPadOff=0")
           , ("M-m", spawn "banshee")
           , ("M-p", spawn "synapse") ]

myStartupHook :: X ()
myStartupHook = do
    spawn "~/scripts/startup.sh"

