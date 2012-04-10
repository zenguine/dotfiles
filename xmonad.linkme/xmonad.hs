import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Util.Run(spawnPipe)
import XMonad.Util.EZConfig(additionalKeys)
import qualified XMonad.StackSet as W
import XMonad.Util.EZConfig
import System.IO

-- make sure to edit paths to xmobar and .xmobarrc to match your system.
    -- If xmobar is in your $PATH, and its config is in ~/.xmobarrc you don't
    -- need the xmobar path or config file, use: xmproc <- spawnPipe "xmobar"
 
myLayout = tiled ||| Mirror tiled ||| Full  
 where  
      -- default tiling algorithm partitions the screen into two panes  
      tiled = Tall nmaster delta ratio  
   
      -- The default number of windows in the master pane  
      nmaster = 1  
   
      -- Default proportion of screen occupied by master pane  
      ratio = 2/3  
   
      -- Percent of screen to increment by when resizing panes  
      delta = 5/100 

-- Define terminal
myTerminal = "urxvt"

-- Define amount and names of workspaces
myWorkspaces = ["1:main","2:dev","3:www","4:media","5:mail","6:chat"]

-- Define the workspace each application goes to
myManageHook = composeAll
   [className =? "Firefox" --> doShift "3:www",
   className =? "Gvim" --> doShift "2:dev",
   className =? "Emacs" --> doShift "2:dev",
   className =? "Clementine" --> doShift "4:media",
   resource =? "desktop_window" --> doIgnore,
   className =? "Xmessage" --> doFloat]

main = do
    xmproc <- spawnPipe "xmobar /home/jcullen/.xmobarrc"
    xmonad $ defaultConfig
        { manageHook = myManageHook <+> manageDocks <+> manageHook defaultConfig
        , layoutHook = avoidStruts myLayout
        , borderWidth = 2
        , normalBorderColor = "#E6E6E6"
        , focusedBorderColor = "#BF00FF"
        , startupHook = startup
        , terminal = myTerminal
        , workspaces = myWorkspaces
        , logHook = dynamicLogWithPP xmobarPP
                        { ppOutput = hPutStrLn xmproc
                        , ppTitle = xmobarColor "green" "" . shorten 50
                        }
        , modMask = mod4Mask     -- Rebind Mod to the Windows key
        } `additionalKeys`
        [ ((mod4Mask .|. shiftMask, xK_z), spawn "xscreensaver-command -lock")
        , ((controlMask, xK_Print), spawn "sleep 0.2; scrot -s")
        , ((0, xK_Print), spawn "scrot")
        ]

startup :: X ()
startup = do spawn "startup.sh"


