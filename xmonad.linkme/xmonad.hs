-- this xmonad configuration is heavily modified from the base template:
-- * it uses mod+H/K/J/L to move between windows,
--   and mod+ctrl+H/J/K/L to move them around
-- * it uses mod+ENTER to go to a full-screen mode
-- * it uses mod+TAB to swap workspaces
-- * it uses mod+B/N to move up/down windows in tabbed/accordian
-- * use mod+shift+J/K to resize
-- * use alt+s to start a shell command 
-- * setup to have accordian, mirror and tabbed layouts as alternatives
--
--  # my .xinitrc
--  xmodmap -e "keysym Caps_Lock = Escape"
--  xmodmap -e "clear lock"
--  xmodmap -e "clear mod1"
--  xmodmap -e "clear mod4"
--  xmodmap -e "add mod1 = Super_L Super_R Alt_R"
--  xmodmap -e "add mod4 = Alt_L"
--  konsole &
--  exec /usr/local/bin/xmonad

import Data.Ratio ((%))
import System.IO

import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.UrgencyHook
import XMonad.Util.Run(spawnPipe)
import XMonad.Util.EZConfig(additionalKeys)
import qualified XMonad.StackSet as W
import XMonad.Util.EZConfig


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
import XMonad.Layout.Spacing
import XMonad.Layout.IM

import XMonad.Prompt
import XMonad.Prompt.Man
import XMonad.Prompt.AppendFile
import XMonad.Prompt.RunOrRaise
import XMonad.Prompt.Shell

import XMonad.Actions.SpawnOn

---- below this was here before --

import XMonad
import System.Exit
import XMonad.Layout.NoBorders
import XMonad.Layout.WindowNavigation
import XMonad.Layout.ToggleLayouts
import XMonad.Layout.Tabbed
import XMonad.Layout.Accordion
import XMonad.Actions.CycleWS
import XMonad.Prompt
import XMonad.Prompt.Shell

import qualified XMonad.StackSet as W
import qualified Data.Map        as M

-- below this new added
import XMonad.Util.WorkspaceCompare
import XMonad.Operations

-- Replace old XMonad.Actions.RotView functionality with newer cycleWS
rotView b = do 
                t <- findWorkspace getSortByTag (bToDir b) NonEmptyWS 1
                windows . W.greedyView $ t
   where bToDir True  = Next
         bToDir False = Prev

------------------------------------------------------------------------
myKeys conf@(XConfig {XMonad.modMask = modMask}) = M.fromList $
    [ ((modMask .|. shiftMask, xK_Return), spawn $ XMonad.terminal conf)

    -- keys for Window Navigation hook
    , ((modMask,                 xK_Right), sendMessage $ Go R)
    , ((modMask,                 xK_Left ), sendMessage $ Go L)
    , ((modMask,                 xK_Up   ), sendMessage $ Go U)
    , ((modMask,                 xK_Down ), sendMessage $ Go D)
    , ((modMask,                 xK_l    ), sendMessage $ Go R)
    , ((modMask,                 xK_h    ), sendMessage $ Go L)
    , ((modMask,                 xK_k    ), sendMessage $ Go U)
    , ((modMask,                 xK_j    ), sendMessage $ Go D)
    , ((modMask .|. controlMask, xK_Right), sendMessage $ Swap R)
    , ((modMask .|. controlMask, xK_Left ), sendMessage $ Swap L)
    , ((modMask .|. controlMask, xK_Up   ), sendMessage $ Swap U)
    , ((modMask .|. controlMask, xK_Down ), sendMessage $ Swap D)
    , ((modMask .|. controlMask, xK_l    ), sendMessage $ Swap R)
    , ((modMask .|. controlMask, xK_h    ), sendMessage $ Swap L)
    , ((modMask .|. controlMask, xK_k    ), sendMessage $ Swap U)
    , ((modMask .|. controlMask, xK_j    ), sendMessage $ Swap D)
    , ((modMask,               xK_Return), sendMessage ToggleLayout)
    , ((modMask,               xK_Tab   ), rotView True)
    , ((modMask .|. shiftMask, xK_Tab   ), rotView False)
    --
    , ((modMask,               xK_c     ), kill)
    , ((modMask .|. shiftMask, xK_c     ), kill)
    , ((modMask,               xK_x     ), 
        spawn "exe=`dmenu_path | dmenu` && eval \"exec $exe\"")
    , ((modMask,               xK_t     ), spawn $ XMonad.terminal conf)
    , ((modMask,               xK_f     ), refresh)
    , ((modMask,               xK_m     ), windows W.focusMaster  )
    , ((modMask,               xK_b     ), windows W.focusUp )
    , ((modMask,               xK_n     ), windows W.focusDown )
    , ((modMask,               xK_space ), sendMessage NextLayout)
    , ((modMask .|. shiftMask, xK_space ), setLayout $ XMonad.layoutHook conf)
    , ((modMask .|. shiftMask, xK_h     ), sendMessage Shrink)
    , ((modMask .|. shiftMask, xK_l     ), sendMessage Expand)
    , ((modMask,               xK_v     ), withFocused $ windows . W.sink)
    , ((modMask,               xK_s     ), shellPrompt defaultXPConfig)

    -- -- standard ones that I didn't modify
    -- , ((modMask              , xK_comma ), sendMessage (IncMasterN -1))
    -- , ((modMask              , xK_period), sendMessage (IncMasterN (-1)))
    -- , ((modMask              , xK_z     ),
    --       modifyGap (\i n -> let x = (XMonad.defaultGaps 
    --                                   conf ++ repeat (0,0,0,0)) !! i
    --                          in if n == x then (0,0,0,0) else x))
    , ((modMask .|. shiftMask, xK_q     ), io (exitWith ExitSuccess))
    , ((modMask              , xK_q     ),
          broadcastMessage ReleaseResources >> restart "xmonad" True)
    ]
    ++

    --
    -- mod-[1..9], Switch to workspace N
    -- mod-shift-[1..9], Move client to workspace N
    --
    [((m .|. modMask, k), windows $ f i)
        | (i, k) <- zip (XMonad.workspaces conf) [xK_1 .. xK_9]
        , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]]
    ++

    --
    -- mod-{w,e,r}, Switch to physical/Xinerama screens 1, 2, or 3
    -- mod-shift-{w,e,r}, Move client to screen 1, 2, or 3
    --
    [((m .|. modMask, key), screenWorkspace sc >>= flip whenJust (windows . f))
        | (key, sc) <- zip [xK_w, xK_e, xK_r] [0..]
        , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]]


------------------------------------------------------------------------
-- Mouse bindings: default actions bound to mouse events
--
myMouseBindings (XConfig {XMonad.modMask = modMask}) = M.fromList $
    [ ((modMask, button1), (\w -> focus w >> mouseMoveWindow w))
    , ((modMask, button2), (\w -> focus w >> windows W.swapMaster))
    , ((modMask, button3), (\w -> focus w >> mouseResizeWindow w))
    ]

------------------------------------------------------------------------
-- Layouts:

-- You can specify and transform your layouts by modifying these values.
-- If you change layout bindings be sure to use 'mod-shift-space' after
-- restarting (with 'mod-q') to reset your layout state to the new
-- defaults, as xmonad preserves your old layout settings by default.
--
-- The available layouts.  Note that each layout is separated by |||,
-- which denotes layout choice.
--
myLayout = toggle $ navkey $ tiled 
           ||| Mirror tiled
           ||| Accordion
           ||| tabbed shrinkText defaultTheme
  where
     -- default tiling algorithm partitions the screen into two panes
     tiled   = Tall nmaster delta ratio
     -- The default number of windows in the master pane
     nmaster = 1
     -- Default proportion of screen occupied by master pane
     ratio   = 1/2
     -- Percent of screen to increment by when resizing panes
     delta   = 3/100
     -- hooks for navigation and full-screen keys
     toggle = toggleLayouts (noBorders Full)
     navkey = configurableNavigation (navigateColor "#111111")

------------------------------------------------------------------------
-- Window rules:

-- Execute arbitrary actions and WindowSet manipulations when managing
-- a new window. You can use this to, for example, always float a
-- particular program, or have a client always appear on a particular
-- workspace.
--
-- To find the property name associated with a program, use
-- > xprop | grep WM_CLASS
-- and click on the client you're interested in.
--

myManageHook = composeAll
   [className =? "Firefox" --> doShift "www",
   className =? "Gvim" --> doShift "dev",
   className =? "Emacs" --> doShift "dev",
   className =? "Clementine" --> doShift "media",
   resource =? "desktop_window" --> doIgnore,
   className =? "Xmessage" --> doFloat,
   className =? "Pidgin" --> doShift "chat"]

------------------------------------------------------------------------
-- Status bars and logging

-- Perform an arbitrary action on each internal state change or X event.
-- See the 'DynamicLog' extension for examples.
--
-- To emulate dwm's status bar
--
-- > logHook = dynamicLogDzen
--

myLogHook h = dynamicLogWithPP xmobarPP
                { ppOutput = hPutStrLn h
                , ppTitle = xmobarColor "purple" "" . shorten 50
                , ppUrgent = xmobarColor "yellow" "red" . xmobarStrip
                }

-- Define amount and names of workspaces
myWorkspaces = ["main","dev","www","media", "chat"] ++ (map show [5..10])

------------------------------------------------------------------------
-- Now run xmonad with all the defaults we set up.

-- Run xmonad with the settings you specify. No need to modify this.
--
    --
main = do 
        xmproc <- spawnPipe "xmobar /home/jcullen/.xmobarrc"
        xmonad $ withUrgencyHook dzenUrgencyHook { args = ["-bg", "darkgreen", "-xs", "1"] } $ defaults xmproc
--
-- A structure containing your configuration settings, overriding
-- fields in the default config. Any you don't override, will 
-- use the defaults defined in xmonad/XMonad/Config.hs
-- 
-- No need to modify this.
--

-- Define terminal
myTerminal = "urxvt"

defaults h = defaultConfig {
      -- simple stuff
        terminal           = myTerminal,
        borderWidth        = 3,
        modMask            = mod4Mask,
        workspaces         = myWorkspaces,
        normalBorderColor  = "#dddddd",
        focusedBorderColor = "#ff0000",

      -- key bindings
        keys               = myKeys,
        mouseBindings      = myMouseBindings,

      -- hooks, layouts
        layoutHook         = avoidStruts $ myLayout,
        manageHook         = myManageHook,
        logHook            = myLogHook h
    } `additionalKeysP`
    [ 

    ("M4-<Backspace>", focusUrgent)

    -- keybindings for common programs
    , ("M-x f", spawnOn "www" "firefox")                             
    , ("M-x b", spawnOn "media" "banshee")                             
    , ("M-x p", spawnOn "main" "pcmanfm")                             

    -- Prompt keybindings
    , ("M4-w m", manPrompt defaultXPConfig)
    , ("M4-w o", runOrRaisePrompt defaultXPConfig)
    , ("M4-w i", shellPrompt defaultXPConfig)

    , ("M4-w t", do
            spawn ("echo -n '* [ ] '>>"++"~/vimwiki/Tasks.wiki")
            appendFilePrompt defaultXPConfig "~/vimwiki/Tasks.wiki"
        )

    , ("M4-n", do 
            spawn ("date>>"++"~/vimwiki/Scratchpad.wiki")
            appendFilePrompt defaultXPConfig "~/vimwiki/Scratchpad.wiki"
        )

    -- volume control
    , ("M4-S-v", spawn "urxvt -e alsamixer")
    , ("<XF86AudioMute>", spawn "amixer -q set Master toggle")
    , ("<XF86AudioLowerVolume>", spawn "amixer -q set Master 5%- unmute")
    , ("<XF86AudioRaiseVolume>", spawn "amixer -q set Master 5%+ unmute")
    ]                  


