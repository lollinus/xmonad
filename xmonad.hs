import XMonad

import XMonad.Config.Gnome
-- import XMonad.Config.Desktop
import XMonad.Actions.CycleWS

import XMonad.Util.EZConfig
import XMonad.Util.Loggers
import XMonad.Util.Ungrab

import Data.Monoid (Endo)
import qualified Data.Map as M

-- import qualified XMonad.Layout.HintedTile as HintedTile
import XMonad.Layout.Hidden
import qualified XMonad.Layout.Magnifier as Mag
import XMonad.Layout.MultiColumns
import XMonad.Layout.NoBorders
import XMonad.Layout.PerWorkspace (onWorkspace)
import XMonad.Layout.ResizableTile
import XMonad.Layout.ThreeColumns
-- import XMonad.Layout.Tabbed

import XMonad.Hooks.DynamicLog
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.SetWMName
import XMonad.Hooks.StatusBar
-- import XMonad.Hooks.StatusBar.PP
import XMonad.Hooks.UrgencyHook

import qualified XMonad.StackSet as S
import Control.Concurrent (threadDelay)

main :: IO ()
main = xmonad . ewmhFullscreen . ewmh
  . withEasySB (statusBarProp "xmobar ~/.config/xmobar/xmobarrc" (pure kbXmobarPP)) defToggleStrutsKey
  $ kbGnomeConfig

-- where
--   toggleStrutsKey :: XConfig Layout -> (KeyMask, KeySym)
--   toggleStrutsKey XConfig{ modMask = m } = (m , xK_b)

kbDefaultLayouts = smartBorders $ avoidStruts $ hiddenWindows $

  ResizableTall nmaster delta ratio []
  -- tiled

  -- ||| Mirror tiled
  -- Full layout makes every window full screen. When you toggle the
  -- active window, it will bring the active window to the front.
  ||| noBorders Full

  ||| threeCol
  ||| zoomThreeCol
  -- ||| hintedTile HintedTile.Wide
  where
    -- ThreeColMid layout puts the large master window in the center
    -- of the screen. As configured below, by default it takes of 3/4 of
    -- the available space. Remaining windows tile to both the left and
    -- right of the master window. You can resize using "super-h" and
    -- "super-l".
    threeCol = ThreeColMid nmaster delta ratio
    -- Three Column layout with zoomable focused window
    zoomThreeCol = Mag.magnifiercz' 1.3 $ ThreeColMid nmaster delta ratio
    
    -- tiled = HintedTile.HintedTile nmaster delta ratio HintedTile.TopLeft HintedTile.Tall
    -- hintedTile = HintedTile.HintedTile nmaster delta ratio HintedTile.TopLeft
    nmaster = 1 -- Default number of windows in the master pane
    ratio = 1/2 -- Default proportion of screen occupied by master pane
    delta = 3/100 -- Percent of screen to increment by when resizing panes


-- Here we define some layouts which will be assigned to specific
-- workspaces based on the functionality of that workspace.

chatLayout = smartBorders $ avoidStruts $ hiddenWindows . Mag.magnifiercz 1.5 $ multiCol [1] 1 0.01 (-0.5)
-- The GIMP layout uses the ThreeColMid layout. The traditional GIMP
-- floating panels approach is a bit of a challenge to handle with xmonad;
-- I find the best solution is to make the image you are working on the
-- master area, and then use this ThreeColMid layout to make the panels
-- tile to the left and right of the image. If you use GIMP 2.8, you
-- can use single-window mode and avoid this issue.
gimpLayout = smartBorders $ avoidStruts $ ThreeColMid 1 (3/100) (3/4)

kbLayouts =
  onWorkspace "7:Chat" chatLayout
  $ onWorkspace "9:Pix" gimpLayout
  $ kbDefaultLayouts

{-
  Workspace configuration. Here you can change the names of your
  workspaces. Note that they are organized in a grid corresponding
  to the layout of the number pad.

  I would recommend sticking with relatively brief workspace names
  because they are displayed in the xmobar status bar, where space
  can get tight. Also, the workspace labels are referred to elsewhere
  in the configuration file, so when you change a label you will have
  to find places which refer to it and make a change there as well.

  This central organizational concept of this configuration is that
  the workspaces correspond to keys on the number pad, and that they
  are organized in a grid which also matches the layout of the number pad.
  So, I don't recommend changing the number of workspaces unless you are
  prepared to delve into the workspace navigation keybindings section
  as well.
-}

kbWorkspaces =
  [
    "1:Term",  "2:Hub", "3:Mail",
    "4:Docs",  "5:Dev", "6:Web",
    "7:Chat",  "8:Dbg", "9:Pix",
    "0:VM",    "Extr1", "Extr2"
  ]

startupWorkspace = "5:Dev"  -- which workspace do you want to be on after launch?

-- Mod4 is the Super / Windows key
kbModMask = mod4Mask
altMask = mod1Mask

-- keybindings
kbAdditionalKeysP = 
  [ ("M-S-z", spawn "xscreensaver-command -lock")
  , ("M-C-s", unGrab *> spawn "scrot -s")
  , ("M-p", spawn "synapse")
  , ("M-\\", withFocused hideWindow)
  , ("M-S-\\", popOldestHiddenWindow)
  , ("M-u", focusUrgent)
  -- , ("<Printscreen>", spawn "xfce4-screenshooter")
  , ("M-d", spawn "dmenu_run -b")
  ]

kbAdditionalKeys =
  [ ((0, xK_Print), spawn "xfce4-screenshooter")
  , ((kbModMask                              , xK_Escape), spawn "/home/karolbarski/bin/layout_switch.sh")
  , ((kbModMask .|. controlMask .|. shiftMask, xK_minus), sendMessage Mag.MagnifyMore)
  , ((kbModMask .|. controlMask              , xK_minus), sendMessage Mag.MagnifyLess)
  , ((kbModMask .|. controlMask              , xK_o    ), sendMessage Mag.ToggleOff  )
  , ((kbModMask .|. controlMask .|. shiftMask, xK_o    ), sendMessage Mag.ToggleOn   )
  , ((kbModMask .|. controlMask              , xK_m    ), sendMessage Mag.Toggle     )
  ]

-- kbKeys conf = M.fromList $ --
addlKeys conf@(XConfig {modMask = modm}) = M.fromList $
  [ ((modm, xK_F1), gnomeMenu) ]

gnomeMenu :: X ()
gnomeMenu = withDisplay $ \dpy -> do
    rw <- asks theRoot
    gnome_panel <- getAtom "_GNOME_PANEL_ACTION"
    panel_menu <- getAtom "_GNOME_PANEL_ACTION_MAIN_MENU"

    -- a "magic" delay that just makes this work. The problem
    -- is specified at
    -- https://code.google.com/p/xmonad/issue/detail?id=451
    -- Increase the delay if it doesn't work for you.
    io $ threadDelay 2000000

    io $ allocaXEvent $ \e -> do
        setEventType e clientMessage
        setClientMessageEvent e rw gnome_panel 32 panel_menu 0
        sendEvent dpy rw False structureNotifyMask e
        sync dpy False

{--
kbKeys conf = M.fromList $
  [ ("M-<Return>", spawn $ XMonad.terminal conf)
  , ("M-r", gnomeRun)
  , ("M-c"     , kill)
  , ("M-<Space>", sendMessage NextLayout)
  , ("M-n", refresh)
  , ("M-m"     , windows S.swapMaster)
  , ("M1-<Tab>", windows S.focusDown)
  , ("M1-S-<Tab>", windows S.focusUp)
  , ("M-<Down>", windows S.swapDown)
  , ("M-<Up>", windows S.swapUp)
  , ("M-<Left>", sendMessage Shrink)
  , ("M-<Right>", sendMessage Expand)
  , ("M-t", withFocused $ windows . S.sink)
  , ("M-w", sendMessage (IncMasterN 1))
  , ("M-v", sendMessage (IncMasterN (-1)))
  , ("M-q"     , broadcastMessage ReleaseResources >> restart "xmonad" True)
  , ("M-S-q", spawn "gnome-session-save --kill")
  , ("M-C-<Left>", prevWS)
  , ("M-C-<Right>", nextWS)    
  ] ++
  -- [
  --   ()
  -- ] ++
  kbAdditionalKeys ++
  -- Alt+F1..F10 switches to workspace
  -- (Alt is in a nicer location for the thumb than the Windows key,
  -- and 1..9 keys are already in use by Firefox, irssi, ...)
  [ ((altMask, k), windows $ S.greedyView i)
      | (i, k) <- zip kbWorkspaces workspaceKeys
  ] ++
  -- mod+F1..F10 moves window to workspace and switches to that workspace
  [ ((kbModMask, k), (windows $ S.shift i) >> (windows $ S.greedyView i))
      | (i, k) <- zip kbWorkspaces workspaceKeys
  ]
  where workspaceKeys = [xK_F1 .. xK_F10]
--}

kbAddKeys conf = M.fromList $ kbAdditionalKeys
kbAddKeysP conf = M.fromList $ kbAdditionalKeysP

kbGnomeConfig = gnomeConfig
  { modMask    = kbModMask -- Rebind Mod to the Super key
  -- add manage hooks while still ignoring panels and using default manageHooks
  , manageHook = manageDocks <+> kbManageHook <+> manageHook gnomeConfig

  , startupHook = setWMName "LG3D"
  -- add a fullscreen tabbed layout that does not avoid covering up
  -- desktop panels before the desktop layouts
  -- , layoutHook = simpleTabbed ||| layoutHook gnomeConfig
  , layoutHook = desktopLayoutModifiers $ kbLayouts
  , logHook = do
      dynamicLogWithPP xmobarPP
      -- updatePointer (Relative 0.9 0.9)
      logHook gnomeConfig
  , keys = addlKeys <+> kbAddKeys <+> keys gnomeConfig
  , workspaces = kbWorkspaces
  }
  `additionalKeysP` kbAdditionalKeysP
  -- `additionalKeys` kbAdditionalKeys

kbConfig = def
  { modMask    = kbModMask -- Rebind Mod to the Super key
  , layoutHook = kbLayouts
  -- , layoutHook = kbDefaultLayouts -- Use custom layouts
  , manageHook = manageDocks <+> kbManageHook 
  , startupHook = setWMName "LG3D"
  , workspaces = kbWorkspaces
  , terminal = "gnome-terminal"
  , keys = addlKeys <+> kbAddKeys <+> keys gnomeConfig
  }
  `additionalKeysP` kbAdditionalKeysP
  -- `additionalKeys` kbAdditionalKeys

kbSafeConfig = def
  { modMask    = kbModMask -- Rebind Mod to the Super key
  , layoutHook = kbLayouts
  -- , layoutHook = kbDefaultLayouts -- Use custom layouts
  , manageHook = manageDocks <+> kbManageHook 
  , startupHook = setWMName "LG3D"
  , workspaces = kbWorkspaces
  , terminal = "gnome-terminal"
  , keys  = addlKeys <+> kbAddKeys <+> keys gnomeConfig
  }
  `additionalKeysP` kbAdditionalKeysP
  -- `additionalKeys` kbAdditionalKeys

kbManageHook :: XMonad.Query (Endo WindowSet)
kbManageHook = composeAll . concat $
  [
    [isFullscreen --> doFullFloat] -- For Media Players
  , [resource =? c --> doF (S.shift "media") | c <- kbClassMediaShifts]
  , [isIM --> moveToIM]
  ]
  where
    isIM               = foldr1 (<||>) [isSignal, isRingCentral, isTeams]
    moveToIM           = doF $ S.shift "7:Chat"
    -- to acquire className use `xprop | grep 'CLASS'`
    isSignal           = className =? "Signal"
    isRingCentral      = className =? "crx__djdehjanccmnmmoknnajakmkgilglkbk"
    isTeams            = className =? "Microsoft Teams - Preview"
    kbClassMediaShifts = ["mplayer", "vlc"]


kbXmobarPP :: PP
kbXmobarPP = def
  { ppSep             = magenta " ・ "
  , ppTitleSanitize   = xmobarStrip
  -- , ppCurrent         = wrap " " "" . xmobarBorder "Top" "#8be9fd" 2
  , ppCurrent         = yellow . wrap (yellow "[") (yellow "]")
  , ppVisible         = wrap (blue "(") (blue ")")
  , ppLayout          = green
                        . (\l -> case l of
                              "Hidden Magnifier NoMaster ThreeCol" -> "|M|"
                              "Hidden ThreeCol"                    -> "||"
                              "Hidden ResizableTall"               -> "|-"
                              "Hidden Tall"                        -> "|-"
                              "Hidden Wide"                        -> "="
                              "Hidden Mirror Tall"                 -> "T" 
                              "Hidden Full"                        -> "[ ]"
                              _                                    -> l
                          )
  , ppHidden          = white . wrap " " ""
  , ppUrgent          = red . wrap (yellow "!") (yellow "!")
  , ppOrder           = \[ws, l, _, wins] -> [ws, l, wins]
  , ppExtras          = [logTitles formatFocused formatUnfocused]
  }
  where
    formatFocused   = wrap (white    "[") (white    "]") . magenta . ppWindow
    formatUnfocused = wrap (lowWhite "[") (lowWhite "]") . blue    . ppWindow

            -- | Window should have *some* title, which should not exceed a sane length
ppWindow :: String -> String
ppWindow = xmobarRaw . (\w -> if null w then "untitled" else w) . shorten 20

blue, green, lowWhite, magenta, red, white, yellow :: String -> String
blue     = xmobarColor "#00bfff" ""
green    = xmobarColor "#00fa9a" ""
magenta  = xmobarColor "#ff79c6" ""
white    = xmobarColor "#f8f8f2" ""
lowWhite = xmobarColor "#bbbbbb" ""
yellow   = xmobarColor "#f1fa8c" ""
red      = xmobarColor "#ff5555" ""
