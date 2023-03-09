import XMonad

import XMonad.Util.EZConfig
import XMonad.Util.Loggers
import XMonad.Util.Ungrab

import Data.Monoid (Endo)

import XMonad.Layout.ThreeColumns
-- import qualified XMonad.Layout.HintedTile as HintedTile
import XMonad.Layout.Magnifier
import XMonad.Layout.Hidden
import XMonad.Layout.NoBorders
import XMonad.Layout.ResizableTile

import XMonad.Hooks.DynamicLog
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.SetWMName
import XMonad.Hooks.StatusBar
-- import XMonad.Hooks.StatusBar.PP
import XMonad.Hooks.UrgencyHook

import qualified XMonad.StackSet as W

main :: IO ()
main = xmonad . ewmhFullscreen . ewmh . withEasySB (statusBarProp "xmobar ~/.config/xmobar/xmobarrc" (pure kbXmobarPP)) defToggleStrutsKey $ kbConfig

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
    zoomThreeCol = magnifiercz' 1.3 $ ThreeColMid nmaster delta ratio
    
    -- tiled = HintedTile.HintedTile nmaster delta ratio HintedTile.TopLeft HintedTile.Tall
    -- hintedTile = HintedTile.HintedTile nmaster delta ratio HintedTile.TopLeft
    nmaster = 1 -- Default number of windows in the master pane
    ratio = 1/2 -- Default proportion of screen occupied by master pane
    delta = 3/100 -- Percent of screen to increment by when resizing panes
    
kbConfig = def
  { modMask= mod4Mask -- Rebind Mod to the Super key
  , layoutHook = kbDefaultLayouts -- Use custom layouts
  , manageHook = manageDocks <+> kbManageHook 
  , startupHook = setWMName "LG3D"
  }
  `additionalKeysP`
  [ ("M-S-z", spawn "xscreensaver-command -lock")
  , ("M-C-s", unGrab *> spawn "scrot -s")
  , ("M-e" , spawn "/opt/microsoft/msedge/msedge")
  , ("M-p", spawn "synapse")
  , ("M-\\", withFocused hideWindow)
  , ("M-S-\\", popOldestHiddenWindow)
  , ("M-u", focusUrgent)
  -- , ("<Printscreen>", spawn "xfce4-screenshooter")
  , ("M-d", spawn "dmenu_run -b")
  ]
  `additionalKeys`
  [ ((0, xK_Print), spawn "xfce4-screenshooter")
  ]

kbManageHook :: XMonad.Query (Endo WindowSet)
kbManageHook = composeAll . concat $
  [
    [isFullscreen --> doFullFloat] -- For Media Players
  , [resource =? c --> doF (W.shift "media") | c <- kbClassMediaShifts]
  ]
  where
    viewShift = doF
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
                              "Hidden Magnifier NoMaster ThreeCol" -> "||"
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
