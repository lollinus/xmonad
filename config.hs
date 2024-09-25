{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, PatternGuards, NoMonomorphismRestriction, OverloadedStrings, DeriveDataTypeable #-}

import XMonad

import System.Exit
import System.Directory             ( doesFileExist )
import Graphics.X11.ExtraTypes.XF86
import XMonad.Util.NamedActions     ( (^++^)
                                    , NamedAction (..)
                                    , addDescrKeys'
                                    , addName
                                    , showKm
                                    , subtitle
                                    )
import XMonad.Util.NamedScratchpad  ( NamedScratchpad(..)
                                    , customFloating
                                    , defaultFloating
                                    , namedScratchpadAction
                                    , namedScratchpadManageHook
                                    )
import XMonad.Util.Run              ( safeSpawn
                                    , spawnPipe
                                    )
-- import XMonad.Util.SpawnOnce        ( spawnOnce )
import XMonad.Util.WorkspaceCompare ( getSortByIndex )

import           Data.Foldable      ( traverse_ )
import           Data.Monoid        (Endo)
import qualified Data.Map as M

-- import qualified XMonad.Layout.HintedTile as HintedTile
import           XMonad.Layout.Gaps                   ( gaps )
import           XMonad.Layout.Grid                   ( Grid(..) )
import           XMonad.Layout.Hidden
import qualified XMonad.Layout.Magnifier as Mag
import           XMonad.Layout.MultiColumns
import           XMonad.Layout.MultiToggle           ( Toggle(..)
                                                     , mkToggle
                                                     , single
                                                     )
import           XMonad.Layout.MultiToggle.Instances ( StdTransformers(NBFULL) )
import           XMonad.Layout.Spacing               ( spacing )
import           XMonad.Layout.NoBorders
import           XMonad.Layout.PerWorkspace          (onWorkspace)
import           XMonad.Layout.ResizableTile
import           XMonad.Layout.Tabbed
import           XMonad.Layout.ThreeColumns
import           XMonad.Prompt                       ( XPConfig(..)
                                                     , amberXPConfig
                                                     , XPPosition(CenteredAt)
                                                     )
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.FadeInactive        ( fadeInactiveLogHook )
import XMonad.Hooks.InsertPosition      ( Focus(Newer)
                                        , Position(Below)
                                        , insertPosition
                                        )
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.UrgencyHook         ( UrgencyHook(..)
                                        , withUrgencyHook
                                        , focusUrgent
                                        )
import XMonad.Actions.CycleWS
-- import XMonad.Actions.DynamicProjects   ( Project(..)
--                                         , dynamicProjects
--                                         , switchProjectPrompt
--                                         )
import XMonad.Actions.DynamicWorkspaces ( removeWorkspace )
import XMonad.Actions.FloatKeys         ( keysAbsResizeWindow
                                        , keysResizeWindow
                                        )
-- import XMonad.Actions.Plane             ( planeKeys )
import XMonad.Actions.RotSlaves         ( rotSlavesUp )
import XMonad.Actions.SpawnOn           ( manageSpawn
                                        , spawnOn
                                        )
import XMonad.Actions.WithAll           ( killAll )       

-- Imports for Polybar --
-- require installation 'cabal install --lib utf8-string --package-env=.'
import qualified Codec.Binary.UTF8.String as UTF8
import qualified DBus                     as D
import qualified DBus.Client              as D

import qualified XMonad.StackSet as W
import qualified XMonad.Util.NamedWindows as W

-- import Control.Concurrent (threadDelay)
import Control.Monad      ( replicateM_
                          , unless)
-- import qualified XMonad.Layout.LayoutModifier as XMonad.Layout
-- import GHC.Data.FastString.Env (mkDFsEnv)
-- import XMonad.Actions.GridSelect (bringSelected)

main :: IO ()
main = mkDbusClient >>= main'

main' :: D.Client -> IO ()
main' dbus = do
  xmonad . docks . ewmh . ewmhFullscreen . keybindings 
    . withUrgencyHook LibNotifyUrgencyHook
    $ def
    { modMask            = kbModMask -- Rebind Mod to the Super key
    , focusFollowsMouse  = False
    , clickJustFocuses   = False
    , borderWidth        = 3
    -- , manageHook = manageDocks <+> kbManageHook1
    , workspaces         = kbWorkspaces
    , terminal           = kbTerminal
    , normalBorderColor  = "#dddddd" -- light gray (default)
    , focusedBorderColor = "#1681f2" -- blue
    , mouseBindings      = myMouseBindings
    , layoutHook         = kbLayout
    , manageHook         = kbManageHook
    , logHook            = kbPolybarLogHook dbus
    , startupHook        = myStartupHook
    }
  where
    -- dynProjects = dynamicProjects projects
    keybindings = addDescrKeys' ((kbModMask, xK_F1), showKeybindings) myKeys

-- Perform an arbitrary action each time xmonad starts or is restarted
-- with mod-q.  Used by, e.g., XMonad.Layout.PerWorkspace to initialize
-- per-workspace layout choices.
myStartupHook :: X ()
myStartupHook = startupHook def

-- original idea: https://pbrisbin.com/posts/using_notify_osd_for_xmonad_notifications/
data LibNotifyUrgencyHook = LibNotifyUrgencyHook deriving (Read, Show)

instance UrgencyHook LibNotifyUrgencyHook where
  urgencyHook LibNotifyUrgencyHook w = do
    name     <- W.getName w
    maybeIdx <- W.findTag w <$> gets windowset
    traverse_ (\i -> safeSpawn "notify-send" [show name, "workspace " ++ i]) maybeIdx

----------------------------------------------------------------------------------
--  Polybar settings (needs DBus client).
--
mkDbusClient :: IO D.Client
mkDbusClient = do
   dbus <- D.connectSession
   _ <- D.requestName dbus (D.busName_ "org.xmonad.log") opts
   return dbus
  where
    opts = [D.nameAllowReplacement, D.nameReplaceExisting, D.nameDoNotQueue]

--  -- Emit a DBus signal on log updates
dbusOutput :: D.Client -> String -> IO ()
dbusOutput dbus str =
  let opath  = D.objectPath_ "/org/xmonad/Log"
      iname  = D.interfaceName_ "org.xmonad.Log"
      mname  = D.memberName_ "Update"
      signal = D.signal opath iname mname
      body   = [D.toVariant $ UTF8.decodeString str]
  in  D.emit dbus $ signal { D.signalBody = body }

polybarHook :: D.Client -> PP
polybarHook dbus =
  let wrapper c s | s /= "NSP" = wrap ("%{F" <> c <> "} ") " %{F-}" s
                  | otherwise  = mempty
      blue   = "#00bfff"
      -- blue   = "#2E9AFE"
      gray   = "#7F7F7F"
      orange = "#ea4300"
      purple = "#9058c7"
      red    = "#722222"
  in  def { ppOutput          = dbusOutput dbus
          , ppCurrent         = wrapper blue
          , ppVisible         = wrapper gray
          , ppUrgent          = wrapper orange
          , ppHidden          = wrapper gray
          , ppHiddenNoWindows = wrapper red
          , ppTitle           = wrapper purple . shorten 90
          }

kbPolybarLogHook dbus = kbLogHook <+> dynamicLogWithPP (polybarHook dbus)

--------------------------------------------------------------------------------

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

kbLayout =
  avoidStruts
   . smartBorders
   . fullScreenToggle
   . comLayout
   . devLayout
   . webLayout
   . demoLayout
   . wrkLayout $ (tiled ||| Mirror tiled ||| column3 ||| full)
  where
    -- default tiling algorithm partitions the screen into two panes
    tiled   = gapSpaced 2 $ Tall nmaster delta ratio
    full    = gapSpaced 2 Full
    column3 = gapSpaced 2 $ ThreeColMid nmaster delta ratio
    grid'   = gapSpaced 2 Grid

    -- The default number of windows in the master pane
    nmaster = 1

    -- Default proportions of screen occupied by master pane
    ratio = 1/2

    -- Percent of screen to increment by when resizing panes
    delta = 3/100

    -- Gaps between windows
    kbGaps gap = gaps [(U, gap),(D, gap),(L, gap),(R, gap)]
    gapSpaced g = spacing g . kbGaps g

    -- Per workspace layout
    comLayout = onWorkspace comWs (tiled ||| full)
    devLayout = onWorkspace devWs (column3 ||| full)
    webLayout = onWorkspace webWs (tiled ||| full)
    wrkLayout = onWorkspace wrkWs (tiled ||| full)
    demoLayout = onWorkspace demoWs (grid' ||| full)

    -- Fullscreen
    fullScreenToggle = mkToggle (single NBFULL)

--------------------------------------------------------------------------------
-- Window rules:
--

-- Execute arbitrary actions and WindowSet manipulations when managing
-- a new window. You can use this to, for example, always float a
-- particular program, or have a client always appear on a particular
-- workspace.
--
-- To find the property name associated with a program, use
-- > xprop | grep WM_CLASS
-- and click on the client you're interested in.
--
-- To match on the WM_NAME, you can use 'title' in the same way that
-- 'className' and 'resource' are used below.
--

type AppName      = String
type AppTitle     = String
type AppClassName = String
type AppCommand   = String
type AppId        = String

data App
  = ClassApp AppClassName AppCommand
  | TitleApp AppTitle AppCommand
  | NameApp AppName AppCommand
  deriving Show

audacious       = ClassApp "Audacious"                             "audacious"
bottom          = TitleApp "bottom"                                "alacritty -t bottom -e bottom --color gruvbox --default_widget_type proc"
vlc             = ClassApp "vlc"                                   "vlc"
scr             = ClassApp "SimpleScreenRecorder"                  "simplescreenrecorder"
spotify         = ClassApp "Spotify"                               "spotify"
itunes          = ClassApp "apple-music-for-linux"                 "apple-music-for-linux"

ringCentral_app :: App
ringCentral_app = ClassApp "crx__djdehjanccmnmmoknnajakmkgilglkbk" "microsoft-edge --profile-directory=Default --app-id=djdehjanccmnmmoknnajakmkgilglkbk \"--app-url=https://app.ringcentral.com/?source=pwa\""

teamsBmw :: App
teamsBmw        = ClassApp "teams-bmw"                             "teams-bmw"

teamsCognizant :: App
teamsCognizant  = ClassApp "teams-cognizant"                       "teams-cognizant"

signal_app :: App
signal_app      = ClassApp "Signal"                                "signal-desktop"
nautilus        = ClassApp "org.gnome.Nautilus"                    "nautilus"
forticlient     = ClassApp "FortiClient"                           "forticlient"
ghci            = TitleApp "ghci"                                  "alacritty -t ghci -e ghci"

kbManageHook = manageApps <+> manageSpawn <+> manageScratchpads
 where
   isBrowserDialog = isDialog <&&> className =? "Brave-browser"
   isFileChooserDialog = isRole =? "GtkFileChooserDialog"
   isPopup             = isRole =? "pop-up"
   isSplash            = isInProperty "_NET_WM_WINDOW_TYPE" "_NET_WM_WINDOW_TYPE_SPLASH"
   isRole              = stringProperty "WM_WINDOW_ROLE"
   isIM                = foldr1 (<||>) [isSignal, isRingCentral, isTeams, isTeamsCognizant, isTeamsBmw]
   isSignal            = className =? getAppName signal_app
   isRingCentral       = className =? getAppName ringCentral_app
   isTeams             = className =? "crx__cifhbcnohmdccbgoicgdjpfamggdegmo"
   isTeamsCognizant    = className =? getAppName teamsCognizant
   isTeamsBmw          = className =? getAppName teamsBmw
   tileBelow           = insertPosition Below Newer
   doCalendarFloat     = customFloating (W.RationalRect (11 / 15) (1 / 48) (1 / 4) (1 / 8))
   manageScratchpads   = namedScratchpadManageHook scratchpads
   anyOf :: [Query Bool] -> Query Bool
   anyOf = foldl (<||>) (pure False)
   match :: [App] -> Query Bool
   match = anyOf . fmap isInstance
   moveToIM = doF $ W.shift comWs
   manageApps = composeOne
     [
     -- isInstance calendar             -?> doCalendarFloat
     -- ,
       match [ vlc ]                   -?> doFullFloat
     , resource =? "desktop_window"    -?> doIgnore
     , anyOf [ isBrowserDialog
             , isFileChooserDialog
             , isDialog
             , isPopup
             , isSplash
             ]                         -?> doCenterFloat
     , isFullscreen                    -?> doFullFloat
     , isIM                            -?> moveToIM
     , pure True                       -?> tileBelow
     ]


isInstance (ClassApp c _) = className =? c
isInstance (TitleApp t _) = title =? t
isInstance (NameApp n _)  = appName =? n

getNameCommand (ClassApp n c) = (n, c)
getNameCommand (TitleApp n c) = (n, c)
getNameCommand (NameApp  n c) = (n, c)

getAppName    = fst . getNameCommand
getAppCommand = snd . getNameCommand

scratchpadApp :: App -> NamedScratchpad
scratchpadApp app = NS (getAppName app) (getAppCommand app) (isInstance app) defaultFloating

runScratchpadApp = namedScratchpadAction scratchpads . getAppName

scratchpads = scratchpadApp <$> [ bottom, scr, spotify, itunes, nautilus, ringCentral_app, signal_app, teamsCognizant, teamsBmw, forticlient, ghci ]

--------------------------------------------------------------------------------

-- need to be installed 'sudo snap isntall --classic alacritty'
kbTerminal = "alacritty" -- gnome-terminal"
appLauncher = "rofi -modi drun,ssh,window -show drun -show-icons"
-- appLauncher = "synapse"
-- calcLauncher = "rofi -show calc -modi calc -no-show-match -no-sort"
emojiPicker = "rofi -modi emoji -show emoji -emoji-mode copy"
screenLocker = "multilockscreen -l dim"
playerctl c = "playerctl --player=spotify,%any " <> c

showKeybindings :: [((KeyMask, KeySym), NamedAction)] -> NamedAction
showKeybindings xs =
  let
    filename = "/home/karolbarski/.config/xmonad/keybindings"
    command f = "alacritty -e dialog --title 'XMonad Key Bindings' --colors --hline \"$(date)\" --textbox " ++ f ++ " 50 100"
  in addName "Show Keybindings" $ do
    b <- liftIO $ doesFileExist filename
    unless b $ liftIO (writeFile filename (unlines $ showKm xs))
    spawnOn webWs $ command filename -- show dialog on webWs
    windows $ W.greedyView webWs     -- switch to webWs

myKeys conf@XConfig {XMonad.modMask = modm} =
  keySet "Applications"
    [ key "Slack" (modm, xK_F2) $ spawnOn comWs "slack"
    ] ^++^
  keySet "Audio"
    [ key "Mute"         (0, xF86XK_AudioMute        ) $ spawn "amixer -q set Master toggle"
    , key "Lower volume" (0, xF86XK_AudioLowerVolume ) $ spawn "amixer -q set Master 5%-"
    , key "Raise volume" (0, xF86XK_AudioRaiseVolume ) $ spawn "amixer -q set Master 5%+"
    , key "Play / Pause" (0, xF86XK_AudioPlay        ) $ spawn $ playerctl "play-pause"
    , key "Stop"         (0, xF86XK_AudioPrev        ) $ spawn $ playerctl "previous"
    , key "Previous"     (0, xF86XK_AudioNext        ) $ spawn $ playerctl "next"
    ] ^++^
  keySet "Launchers"
    [ key "Terminal"      (modm .|. shiftMask  , xK_Return  ) $ spawn (XMonad.terminal conf)
    , key "Apps (Rofi)"   (modm                , xK_p       ) $ spawn appLauncher
    -- , key "Calc (Rofi)"   (modm .|. shiftMask  , xK_c       ) $ spawn calcLauncher
    -- , key "Emojis (Rofi)" (modm .|. shiftMask  , xK_m       ) $ spawn emojiPicker
    , key "Lock screen"   (modm .|. controlMask, xK_l       ) $ spawn screenLocker
    ] ^++^
  keySet "Layouts"
    [ key "Next"          (modm              , xK_space     ) $ sendMessage NextLayout
    , key "Reset"         (modm .|. shiftMask, xK_space     ) $ setLayout (XMonad.layoutHook conf)
    , key "Fullscreen"    (modm              , xK_f         ) $ sendMessage (Toggle NBFULL)
    ] ^++^
  keySet "Polybar"
    [ key "Toggle"        (modm              , xK_equal     ) togglePolybar
    ] ^++^
  -- keySet "Projects"
  --   [ key "Switch prompt" (modm              , xK_o         ) $ switchProjectPrompt projectsTheme
  --   ] ^++^
  keySet "Scratchpads"
    [ key "Audacious"       (modm .|. controlMask,  xK_a    ) $ runScratchpadApp audacious
    , key "bottom"          (modm .|. controlMask,  xK_y    ) $ runScratchpadApp bottom
    , key "Files"           (modm .|. controlMask,  xK_f    ) $ runScratchpadApp nautilus
    , key "Screen recorder" (modm .|. controlMask,  xK_r    ) $ runScratchpadApp scr
    , key "FortiClient"     (modm .|. controlMask,  xK_v    ) $ runScratchpadApp forticlient
    -- , key "Teams"           (modm .|. controlMask,  xK_t    ) $ runScratchpadApp teams
    , key "Teams Cognizant" (modm .|. controlMask,  xK_t    ) $ runScratchpadApp teamsCognizant
    , key "Teams Bmw"       (modm .|. controlMask,  xK_T    ) $ runScratchpadApp teamsBmw
    , key "RingCentral"     (modm .|. controlMask,  xK_p    ) $ runScratchpadApp ringCentral_app
    , key "Signal"          (modm .|. controlMask,  xK_s    ) $ runScratchpadApp signal_app
    , key "Ghci"            (modm .|. controlMask,  xK_g    ) $ runScratchpadApp ghci
    ] ^++^
  keySet "Screens" switchScreen ^++^
  keySet "System"
    [ key "Toggle status bar gap"  (modm              , xK_b ) toggleStruts
    , key "Logout (quit XMonad)"   (modm .|. shiftMask, xK_q ) $ io exitSuccess
    , key "Restart XMonad"         (modm              , xK_q ) $ spawn "xmonad --recompile; xmonad --restart"
    , key "Capture entire screen"  (modm          , xK_Print ) $ spawn "flameshot full -p ~/Pictures/flameshot/"
    , key "Screenshot"             (0             , xK_Print ) $ spawn "xfce4-screenshooter"
    -- , key "Switch keyboard layout" (modm          , xK_F8    ) $ spawn "kls"
    , key "Disable CapsLock"       (modm          , xK_F9    ) $ spawn "setxkbmap -option ctrl:nocaps"
    , key "Switch keyboard Layout" (modm          , xK_Escape) $ spawn "/home/karolbarski/bin/layout_switch.sh"
    -- Screen brightness
    , key "Inc screen brightness"  (0, xF86XK_KbdBrightnessUp  ) $ spawn "light -A 10"
    , key "Dec screen brightness"  (0, xF86XK_KbdBrightnessDown) $ spawn "light -U 10"
    ] ^++^
  keySet "Windows"
    [ key "Close focused"   (modm              , xK_BackSpace) kill
    , key "Close all in ws" (modm .|. shiftMask, xK_BackSpace) killAll
    , key "Refresh size"    (modm              , xK_n        ) refresh
    , key "Focus next"      (modm              , xK_j        ) $ windows W.focusDown
    , key "Focus previous"  (modm              , xK_k        ) $ windows W.focusUp
    , key "Focus master"    (modm              , xK_m        ) $ windows W.focusMaster
    , key "Swap master"     (modm              , xK_Return   ) $ windows W.swapMaster
    , key "Swap next"       (modm .|. shiftMask, xK_j        ) $ windows W.swapDown
    , key "Swap previous"   (modm .|. shiftMask, xK_k        ) $ windows W.swapUp
    , key "Shrink master"   (modm              , xK_h        ) $ sendMessage Shrink
    , key "Expand master"   (modm              , xK_l        ) $ sendMessage Expand
    , key "Switch to tile"  (modm              , xK_t        ) $ withFocused (windows . W.sink)
    , key "Rotate slaves"   (modm .|. shiftMask, xK_Tab      ) rotSlavesUp
    , key "Decrease size"   (modm              , xK_d        ) $ withFocused (keysResizeWindow (-10,-10) (1,1))
    , key "Increase size"   (modm              , xK_s        ) $ withFocused (keysResizeWindow (10,10) (1,1))
    , key "Decr abs size"   (modm .|. shiftMask, xK_d        ) $ withFocused (keysAbsResizeWindow (-10,-10) (1024,752))
    , key "Incr abs size"   (modm .|. shiftMask, xK_s        ) $ withFocused (keysAbsResizeWindow (10,10) (1024,752))
    , key "Focus urgent"    (modm              , xK_u        ) focusUrgent
    ] ^++^
  keySet "Magnification"
    [ key "Magnify More"   (modm .|. controlMask .|. shiftMask , xK_minus) $ sendMessage Mag.MagnifyMore 
    , key "Magnify Less"   (modm .|. controlMask               , xK_minus) $ sendMessage Mag.MagnifyLess 
    , key "Magnify Off"    (modm .|. controlMask               , xK_o    ) $ sendMessage Mag.ToggleOff   
    , key "Magnify On"     (modm .|. controlMask .|. shiftMask , xK_o    ) $ sendMessage Mag.ToggleOn    
    , key "Magnify Toggle" (modm .|. controlMask               , xK_m    ) $ sendMessage Mag.Toggle       
    ] ^++^
  keySet "Window Hiding"
    [ key "Hide window"    (modm              , xK_backslash) $ withFocused hideWindow
    , key "UnHide oldest"  (modm .|. shiftMask, xK_backslash) $ popOldestHiddenWindow
    ] ^++^
  keySet "Workspaces"
    [ key "Next"          (modm              , xK_period    ) nextWS'
    , key "Previous"      (modm              , xK_comma     ) prevWS'
    , key "Remove"        (modm .|. shiftMask, xK_F4        ) removeWorkspace
    ] ++ switchWsById
  where
    togglePolybar = spawn "polybar-msg cmd toggle &"
    toggleStruts = togglePolybar >> sendMessage ToggleStruts
    keySet s ks = subtitle s : ks
    key n k a = (k, addName n a)
    action m = if m == shiftMask then "Move to " else "Switch to "
    -- mod-[1..9]: Switch to workspace N | mod-shift-[1..9]: Move client to workspace N
    switchWsById =
      [ key (action m <> show i) (m .|. modm, k) (windows $ f i)
          | (i, k) <- zip (XMonad.workspaces conf) [xK_1 .. xK_9]
          , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]]
      -- mod-{w,e,r}, Switch to physical/Xinerama screens 1, 2, or 3
      -- mod-shift-{w,e,r}, Move client to screen 1, 2, or 3
    switchScreen =
      [ key (action m <> show sc) (m .|. modm, k) (screenWorkspace sc >>= flip whenJust (windows . f))
          | (k, sc) <- zip [xK_w, xK_e, xK_r] [0..]
          , (f, m)  <- [(W.view, 0), (W.shift, shiftMask)]]

----------- Cycle through workspaces one by one but filtering out NSP (scratchpads) -----------

nextWS' = switchWS Next
prevWS' = switchWS Prev

switchWS dir =
  findWorkspace filterOutNSP dir anyWS 1 >>= windows . W.view

filterOutNSP =
  let g f xs = filter (\(W.Workspace t _ _) -> t /= "NSP") (f xs)
  in g <$> getSortByIndex

------------------------------------------------------------------------
-- Mouse bindings: default actions bound to mouse events
--
-- myMouseBindings :: XConfig l -> M.Map (KeyMask, Button) (Window -> X ())
myMouseBindings XConfig {XMonad.modMask = modm} = M.fromList

    -- mod-button1, Set the window to floating mode and move by dragging
    [ ((modm, button1), \w -> focus w >> mouseMoveWindow w
                                      >> windows W.shiftMaster)

    -- mod-button2, Raise the window to the top of the stack
    , ((modm, button2), \w -> focus w >> windows W.shiftMaster)

    -- mod-button3, Set the window to floating mode and resize by dragging
    , ((modm, button3), \w -> focus w >> mouseResizeWindow w
                                      >> windows W.shiftMaster)

    -- you may also bind events to the mouse scroll wheel (button4 and button5)
    ]

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

termWs = "Term"
hubWs  = "Hub"
mailWs = "Mail"
docsWs = "Docs"
comWs  = "Chat"
devWs  = "Dev"
webWs  = "Web"
wrkWs  = "Wrk"
demoWs = "Demo"

kbWorkspaces =
  [termWs, hubWs, mailWs, docsWs, devWs, webWs, comWs, wrkWs, demoWs, "0:VM"]

--------------------------------------------------------------------------------
-- Dynamic Projects
--
-- projects :: [Project]
-- projects =
--   [ Project { projectName = "ID8"
--             , projectDirectory = "~/projects/ID8"
--             , projectStartHook = Just $ spawn kbTerminal
--             }
--   ]

--------------------------------------------------------------------------------

-- Mod4 is the Super / Windows key
kbModMask = mod4Mask

{--
addlKeys conf@(XConfig {modMask = modm}) = M.fromList
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
--}

kbManageHook1 :: XMonad.Query (Endo WindowSet)
kbManageHook1 = composeAll . concat $
  [
    [isFullscreen --> doFullFloat] -- For Media Players
  , [resource =? c --> doF (W.shift "media") | c <- kbClassMediaShifts]
  , [isIM --> moveToIM]
  ]
  where
    isIM               = foldr1 (<||>) [isSignal, isRingCentral, isTeams, isTeamsCognizant, isTeamsBmw]
    moveToIM           = doF $ W.shift comWs
    -- to acquire className use `xprop | grep 'CLASS'`
    isSignal           = className =? getAppName signal_app
    isRingCentral      = className =? "crx__djdehjanccmnmmoknnajakmkgilglkbk"
    isTeams            = className =? "crx__cifhbcnohmdccbgoicgdjpfamggdegmo"
    isTeamsCognizant   = className =? "teams-congizant"
    isTeamsBmw         = className =? "teams-bmw"
    kbClassMediaShifts = ["mplayer", "vlc"]


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

-- projectsTheme :: XPConfig
-- projectsTheme = amberXPConfig
--   { bgHLight = "#002b36"
--   , font     = "xft:Bitstream Vera Sans Mono:size=8:antialias=true"
--   , height   = 50
--   , position = CenteredAt 0.5 0.5
--   }

------------------------------------------------------------------------
-- Status bars and logging

-- Perform an arbitrary action on each internal state change or X event.
-- See the 'XMonad.Hooks.DynamicLog' extension for examples.
--
kbLogHook = fadeInactiveLogHook 0.9
