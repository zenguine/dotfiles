import           XMonad
import           XMonad.Config.Kde
import           XMonad.Layout.NoBorders
import           XMonad.Util.EZConfig

main :: IO ()
main = xmonad $ kde4Config {
  modMask = mod4Mask,
  layoutHook = smartBorders . layoutHook $ kde4Config,
  focusedBorderColor = "purple",
  startupHook = do startupHook kde4Config
                   myStartupHook
  , terminal = "urxvt" }
       `additionalKeysP`
       [ ("M-o", spawn "echo 'ALTERNATE_EDITOR=\"\" emacsclient -c & exit' | zsh")
       , ("M-f", spawn "firefox")
       , ("M-m", spawn "krunner")
       , ("M-p", spawn "dmenu_run")
       , ("M-d", spawn "deluge")
       , ("M-c", spawn "chromium")
       , ("M-s", spawn "spotify")
       , ("M-F1", spawn "synclient TouchPadOff=1")
       , ("M-F2", spawn "synclient TouchPadOff=0")
       ]

myStartupHook :: X ()
myStartupHook = spawn "~/scripts/startup.sh"
