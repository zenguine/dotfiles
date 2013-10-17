import XMonad
import XMonad.Config.Xfce
import XMonad.Hooks.SetWMName

main = xmonad xfceConfig { 
             modMask = mod4Mask
           , startupHook = setWMName "LG3D"
           , terminal = "urxvt"
           }
