import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Util.Run(spawnPipe)
import XMonad.Util.EZConfig(additionalKeys)
import qualified XMonad.StackSet as W
import XMonad.Util.EZConfig
import System.IO

import qualified XMonad.Layout.Magnifier as Mag
import XMonad.Layout.Accordion
import XMonad.Layout.Combo
import XMonad.Layout.Gaps
import XMonad.Layout.Grid
import XMonad.Layout.MultiToggle
import XMonad.Layout.MultiToggle.Instances
import XMonad.Layout.Named
import XMonad.Layout.NoBorders
import XMonad.Layout.PerWorkspace
import XMonad.Layout.Reflect
import XMonad.Layout.ResizableTile
import XMonad.Layout.TwoPane
import XMonad.Layout.WindowNavigation
import XMonad.Layout.WorkspaceDir

import XMonad.Prompt
import XMonad.Prompt.Man
import XMonad.Prompt.AppendFile

import XMonad.Actions.SpawnOn

-- make sure to edit paths to xmobar and .xmobarrc to match your system.
    -- If xmobar is in your $PATH, and its config is in ~/.xmobarrc you don't
    -- need the xmobar path or config file, use: xmproc <- spawnPipe "xmobar"

-- specify a custom layout hook.
myLayoutHook =

    -- automatically avoid overlapping my dzen status bar.
    avoidStrutsOn [U] $                                        -- (3)

    -- make manual gap adjustment possible.
    gaps (zip [U,D,L,R] (repeat 0)) $

    -- start all workspaces in my home directory, with the ability
    -- to switch to a new working dir.                          -- (10,11)
    workspaceDir "~" $

    -- navigate directionally rather than with mod-j/k
    configurableNavigation (navigateColor "#00aa00") $          -- (8)

    -- ability to toggle between fullscreen, reflect x/y, no borders,
    -- and mirrored.
    mkToggle1 NBFULL $                                  -- (14)
    mkToggle1 REFLECTX $                                -- (14,13)
    mkToggle1 REFLECTY $                                -- (14,13)
    mkToggle1 NOBORDERS $                               --  "
    mkToggle1 MIRROR $                                  --  "

    -- borders automatically disappear for fullscreen windows.
    smartBorders $                                              -- (7)

    -- "web" and "irc" start in Full mode and can switch to tiled...
    onWorkspaces ["www","media"] (Full ||| myTiled) $               -- (10,0)

    -- ...whereas all other workspaces start tall and can switch
    -- to a grid layout with the focused window magnified.
    myTiled |||           -- resizable tall layout
    Mag.magnifier Grid |||                                      -- (15,6)
    -- TwoPane (3/100) (1/2) |||
    (named "Full|Acc" $ combineTwo myTiled Full Accordion)      -- (15b)

-- use ResizableTall instead of Tall, but still call it "Tall".
myTiled = named "Tall" $ ResizableTall 1 0.03 0.5 []            -- (9,5)

-- Define terminal
myTerminal = "urxvt"

-- Define amount and names of workspaces
myWorkspaces = ["main","dev","www","media"] ++ (map show [5..10])

-- Define the workspace each application goes to
myManageHook = composeAll
   [className =? "Firefox" --> doShift "www",
   className =? "Gvim" --> doShift "dev",
   className =? "Emacs" --> doShift "dev",
   className =? "Clementine" --> doShift "media",
   resource =? "desktop_window" --> doIgnore,
   className =? "Xmessage" --> doFloat]

main = do
    xmproc <- spawnPipe "xmobar /home/jcullen/.xmobarrc"
    xmonad $ defaultConfig
        { manageHook = myManageHook <+> manageDocks <+> manageHook defaultConfig
        , layoutHook = avoidStruts myLayoutHook
        , borderWidth = 2
        , normalBorderColor = "#E6E6E6"
        , focusedBorderColor = "#BF00FF"
        , startupHook = startup
        , terminal = myTerminal
        , workspaces = myWorkspaces
        , logHook = dynamicLogWithPP xmobarPP
                        { ppOutput = hPutStrLn xmproc
                        , ppTitle = xmobarColor "purple" "" . shorten 50
                        }
        , modMask = mod4Mask     -- Rebind Mod to the Windows key
        } `additionalKeysP`
        [ ("M4-n", appendFilePrompt defaultXPConfig "/home/jcullen/notes.txt")
        , ("M4-m", manPrompt defaultXPConfig)
        , ("M-C-<Space>", sendMessage $ Toggle NBFULL)              
        , ("M-C-x",       sendMessage $ Toggle REFLECTX)            
        , ("M-C-y",       sendMessage $ Toggle REFLECTY)            
        , ("M-C-m",       sendMessage $ Toggle MIRROR)              
        , ("M-C-b",       sendMessage $ Toggle NOBORDERS)           

        -- keybindings for common programs
        , ("M-x f", spawnOn "www" "firefox")                             
        , ("M-x c", spawnOn "media" "clementine")                             
        , ("M-x p", spawnOn "main" "pcmanfm")                             

        -- window navigation keybindings.
        , ("M4-l", sendMessage $ Go R)                             
        , ("M4-h", sendMessage $ Go L)                             
        , ("M4-k", sendMessage $ Go U)                             
        , ("M4-j", sendMessage $ Go D)                             
        , ("M4-S-l", sendMessage $ Swap R)                         
        , ("M4-S-h", sendMessage $ Swap L)                         
        , ("M4-S-k", sendMessage $ Swap U)                         
        , ("M4-S-j", sendMessage $ Swap D)                         
        , ("M4-S-C-l", sendMessage $ Move R)                         
        , ("M4-S-C-h", sendMessage $ Move L)                         
        , ("M4-S-C-k", sendMessage $ Move U)                         
        , ("M4-S-C-j", sendMessage $ Move D)                         

        , ("M4-S-l", spawn "xscreensaver-command -lock")

        -- volume control
        , ("M4-S-v", spawn "urxvt -e alsamixer")
        , ("<XF86AudioMute>", spawn "amixer -q set Master toggle")
        , ("<XF86AudioLowerVolume>", spawn "amixer -q set Master 5%- unmute")
        , ("<XF86AudioRaiseVolume>", spawn "amixer -q set Master 5%+ unmute")
        ]                  


startup :: X ()
startup = do spawn "~/scripts/startup.sh"


