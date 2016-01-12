import           XMonad
import           XMonad.Config.Gnome
import           XMonad.Layout.NoBorders
import           XMonad.Util.EZConfig

main :: IO ()
main = xmonad $ gnomeConfig {
  modMask = mod4Mask,
  layoutHook = smartBorders . layoutHook $ gnomeConfig,
  focusedBorderColor = "purple",
  startupHook = do startupHook gnomeConfig
                   myStartupHook
  , terminal = "urxvt" }
       `additionalKeysP`
       [ ("M-o", spawn "echo 'ALTERNATE_EDITOR=\"\" emacsclient -c & exit' | zsh")
       , ("M-f", spawn "firefox")
       , ("M-m", spawn "tomahawk")
       , ("M-s", spawn "urxvt")
       , ("M-F1", spawn "synclient TouchPadOff=1")
       , ("M-F2", spawn "synclient TouchPadOff=0")
       ]

myStartupHook :: X ()
myStartupHook = spawn "~/scripts/startup.sh"
