{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE DeriveDataTypeable        #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE LambdaCase                #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE PartialTypeSignatures     #-}
{-# LANGUAGE RecordWildCards           #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TypeApplications          #-}
{-# LANGUAGE TypeOperators             #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-deprecations #-}  -- For custom EWMH event hook
{-# OPTIONS_GHC -Wno-unused-local-binds  #-}

------------------------------------------------------------------------------
-- |
-- Module      : Main
-- Description : XMonad configuration
-- Copyright   : (c) 2011-2021 Samuli Thomasson
-- License     : BSD-3
--
-- Maintainer  : Samuli Thomasson <samuli.thomasson@paivola.fi>
-- Stability   : unstable
-- Portability : non-portable
------------------------------------------------------------------------------


module Main (main) where

import XMonad.Actions.TiledWindowDragging (dragWindow)
import XMonad.Layout.DraggingVisualizer (draggingVisualizer)

import           Prelude

import           XMonad                                hiding (spawn)
import           XMonad.Prelude                        hiding (group)
import qualified XMonad.StackSet                       as W

import qualified XMonad.Actions.CopyWindow             as CW
import           XMonad.Actions.CycleRecentWS          (cycleWindowSets)
import           XMonad.Actions.CycleWS                (WSType(..))
import qualified XMonad.Actions.CycleWS                as CycleWS
import qualified XMonad.Actions.DynamicWorkspaceGroups as WSG
import qualified XMonad.Actions.DynamicWorkspaceOrder  as DO
import qualified XMonad.Actions.DynamicWorkspaces      as DynWS
import qualified XMonad.Actions.FlexibleManipulate     as Flex
import qualified XMonad.Actions.FloatKeys              as FloatKeys
import qualified XMonad.Actions.FloatSnap              as FloatSnap
import qualified XMonad.Actions.GridSelect             as GS
import qualified XMonad.Actions.Minimize
import qualified XMonad.Actions.Navigation2D           as Navigation2D
import           XMonad.Actions.NoBorders              (toggleBorder)
import           XMonad.Actions.OnScreen               (Focus(..), onScreen)
import qualified XMonad.Actions.PhysicalScreens        as PScreen
import qualified XMonad.Actions.RotSlaves              as RotSlaves
import qualified XMonad.Actions.SpawnOn                as SpawnOn
import qualified XMonad.Actions.UpdatePointer          as A (updatePointer)
import XMonad.Actions.AfterDrag (ifClick)
import qualified XMonad.Hooks.EwmhDesktops             as EWMH
import           XMonad.Hooks.FadeWindows              (isFloating)
import qualified XMonad.Hooks.FloatNext                as FloatNext
import qualified XMonad.Hooks.InsertPosition           as InsertPosition
import qualified XMonad.Hooks.ManageDebug
import qualified XMonad.Hooks.ManageDocks              as ManageDocks
import           XMonad.Hooks.ManageHelpers
import           XMonad.Hooks.Minimize                 (minimizeEventHook)
import           XMonad.Hooks.Place                    (placeFocused, placeHook, simpleSmart, smart, underMouse, withGaps)
import qualified XMonad.Hooks.ToggleHook               as ToggleHook
import           XMonad.Hooks.UrgencyHook              (focusUrgent)
import qualified XMonad.Hooks.UrgencyHook              as Urgency
import           XMonad.Hooks.WallpaperSetter
import qualified XMonad.Layout.BinarySpacePartition    as BSP
import           XMonad.Layout.BoringWindows           (boringWindows)
import qualified XMonad.Layout.BoringWindows           as BW
import qualified XMonad.Layout.Fullscreen              as FS
import           XMonad.Layout.GridVariants            (ChangeGridGeom(..), ChangeMasterGridGeom(..), Grid(Grid))
import qualified XMonad.Layout.TallMastersCombo        as TMC
--import qualified XMonad.Layout.GridVariants          as GridV (Orientation(..))
import qualified XMonad.Layout.LayoutHints             as LayoutHints
import qualified XMonad.Layout.Magnifier               as Magnifier
import           XMonad.Layout.Maximize                (maximizeRestore, maximizeWithPadding)
import           XMonad.Layout.Minimize                (minimize)
import qualified XMonad.Layout.Mosaic                  as Mosaic
import qualified XMonad.Layout.MouseResizableTile      as MRT
import qualified XMonad.Layout.MultiToggle             as MultiToggle
import           XMonad.Layout.MultiToggle.Instances
import qualified XMonad.Layout.NoBorders               as NoBorders
import           XMonad.Layout.OneBig                  (OneBig(OneBig))
import           XMonad.Layout.Reflect                 (REFLECTX(..), REFLECTY(..), reflectHoriz)
import           XMonad.Layout.Spacing                 (Border(Border), spacingRaw, toggleScreenSpacingEnabled, toggleWindowSpacingEnabled)
import           XMonad.Layout.ThreeColumns            (ThreeCol(ThreeColMid))
import qualified XMonad.Layout.WindowNavigation        as WindowNavigation
import qualified XMonad.Prompt                         as XP
import           XMonad.Prompt.ConfirmPrompt           (confirmPrompt)
import           XMonad.Prompt.Input                   (inputPromptWithCompl, (?+))
import qualified XMonad.Prompt.Input                   as XP.Input
import qualified XMonad.Prompt.Pass                    as XP.Pass
import qualified XMonad.Prompt.Shell                   as XP.Shell
import qualified XMonad.Prompt.Window                  as XP (WindowPrompt(Goto), allWindows, windowPrompt)
import qualified XMonad.Prompt.Workspace               as XP (Wor(Wor))
import           XMonad.Util.NamedActions              (showKm)
import           XMonad.Util.PureX
import qualified XMonad.Util.Rectangle                 as Rect
import           XMonad.Util.Types                     (Direction1D(..), Direction2D(..))
import qualified XMonad.Util.WindowProperties          as WinProp
import           XMonad.Layout.StateFull (focusTracking)
import           XMonad.Config.Gnome (gnomeRegister)

import qualified Data.List                             as L
import qualified Data.Map                              as M
import           Data.Proxy
import           Data.Ratio                            ((%))
import           System.Directory                      (doesFileExist)
import qualified System.Environment
import           System.Exit                           (exitSuccess)
import           System.FilePath                       ((</>))
import qualified System.Posix                          as Posix
import           Text.Printf                           (printf)
import           Text.Read                             (readMaybe)


import           DesktopEntries
import qualified MyDebug
import           MyRun
import           MyTheme
import qualified MyXmobar
import           Scratchpads
import           SpawnOnByPPID
import           XMonad.Config.CommandsKeysF
import qualified XMonad.Config.CommandsKeysF           as CF
import           XMonad.Hooks.EwmhDesktopsEx           (setEWMHDesktopGeometry)
import           XMonad.Layout.Hinted
import           XMonad.Prompt.Environ                 (environPrompt)
import qualified XMonad.Prompt.Qutebrowser             as XP.QB
import qualified XMonad.Util.DesktopNotifications      as Notify
import qualified XMonad.Util.ExtensibleState           as XS
import           XMonad.Util.Minimize
import           XMonad.Util.NamedCommands
import           XMonad.Util.NamedCommands.Orphans

-- * TOP

main :: IO ()
main = xmonad =<< myConfig

myConfig :: IO (XConfig _)
myConfig =
    -- debugging
  restoreWorkspaces
  -- . urgencyHook Notify.urgencyHook
  -- . myWallpapers
  . applyC (\xc -> xc {  startupHook     = gnomeRegister <+> startupHook xc })
  -- FS.fullscreen
  . applyC (\xc -> xc
      {  handleEventHook = handleEventHook xc <+> FS.fullscreenEventHook
      ,  manageHook      = manageHook xc <+> FS.fullscreenManageHook
      ,  startupHook     = startupHook xc <+> EWMH.fullscreenStartup
      })
  -- EWMH fullscreen
  -- . applyC EWMH.ewmhFullscreen
--  . applyC (\xc -> xc
--      {  handleEventHook = handleEventHook xc <+> myFullscreenEventHook
--      ,  startupHook = startupHook xc <+> EWMH.fullscreenStartup })
  . applyC EWMH.ewmh
  . applyC (\xc -> xc
      { startupHook =
              startupHook xc
              -- <+> Notify.startupHook
              <+> setEWMHDesktopGeometry
              <+> scratchpadsStartupHook myScratchpads
      , handleEventHook =
              LayoutHints.hintsEventHook       -- Refreshes the layout whenever a window changes its hints.
              <+> minimizeEventHook    -- Handle minimize/maximize requests
              <+> removeMinimizedState
              <+> handleEventHook xc
      , logHook =
              logHook xc
              <+> myUpdatePointer (0.5, 0.5) (0.4, 0.4)
      })
  . applyIO (CF.addAll myShowKeys myCmds)
  . statusbars
  . return $ def
  { terminal           = "my-terminal"
  , borderWidth        = 1
  , focusedBorderColor = colCyan
  , normalBorderColor  = colBase02
  , modMask            = mod4Mask
  , focusFollowsMouse  = True
  , clickJustFocuses   = False
  , clientMask         = clientMask def .|. focusChangeMask -- default: structureNotifyMask .|. enterWindowMask .|. propertyChangeMask@
  , rootMask           = rootMask def .|. focusChangeMask -- default: substructureRedirectMask .|. substructureNotifyMask .|. enterWindowMask .|. leaveWindowMask .|. structureNotifyMask .|. buttonPressMask
  , handleEventHook    = myRestartEventHook <> handleEventHook def
  , manageHook         = myManageHook
                          <+> ToggleHook.toggleHook "keepfocus" (InsertPosition.insertPosition InsertPosition.Above InsertPosition.Older) -- default: Above Never
                          <+> SpawnOn.manageSpawn
                          <+> FloatNext.floatNextHook
  , layoutHook = myLayout
  }
    where
    urgencyHook :: (Urgency.UrgencyHook h, LayoutClass l Window) => h -> XConfig' l
    urgencyHook = applyC . flip Urgency.withUrgencyHookC Urgency.urgencyConfig { Urgency.suppressWhen = Urgency.Focused }

    statusbars = applyC $ \xc -> MyXmobar.myStatusBars . ManageDocks.docks $ xc
      -- { handleEventHook = docksEventHookExtra <+> handleEventHook xc }

    debugging = applyC $ \xc -> xc
      { handleEventHook = MyDebug.debugEventHook <+> handleEventHook xc
      , manageHook      = MyDebug.myDebugManageHook <+> XMonad.Hooks.ManageDebug.maybeManageDebug <+> manageHook xc }

    -- Remove destroyed windows from the list of minimized windows, so that the state is reset next time that windowid
    -- is used.
    removeMinimizedState DestroyWindowEvent{..} = do
      XS.modify $ \x@Minimized { rectMap = rm, minimizedStack = ms } -> x { minimizedStack = L.delete ev_window $ minimizedStack x }
      return (All True)

    -- fix 'pinentry-qt' which starts minimized and then uses raising to top as signal to maximize.
    -- Seriously: https://dev.gnupg.org/rP9dd46926f8d50cca059bbf5ea7aa003b9199a05f
    removeMinimizedState ClientMessageEvent{ev_message_type = a, ev_data = vs, ev_window = w}
      | 1:v1:v2:_ <- vs = do
        a' <- getAtom "_NET_WM_STATE"
        v' <- getAtom "_NET_WM_STATE_ABOVE"
        when (a == a' && v' == fromIntegral v1) $ XMonad.Actions.Minimize.maximizeWindow w
        return (All True)
    removeMinimizedState _ = return (All True)

myManageHook :: ManageHook
myManageHook = composeOne
  [ managePads
  , transience
  , appName   =? "term-dialog"     -?> doCenterFloat
  -- , appName   =? "pinentry-qt"     -?> doCenterFloat
  , className =? "feh"             -?> smartPlaceHook (16,5,16,5) (0,0) <+> doFloat
  , className =? "Xmessage"        -?> doCenterFloat
  , className =? "Xmag"            -?> doSideFloat NC
  , className =? "Nvidia-settings" -?> doCenterFloat
  , className =? "zoom" <&&> title /=? "Zoom Meeting" -?> doFloat
  -- , isFullscreen -?> doFullFloat -- For EWMH fullscreen
  , isDialog                       -?> placeHook (underMouse (0.7,0.7)) <+> doFloat
  , isFloating =? False <&&> anyWindowCurWS (isFullscreen <&&> isFloating) -?> doFloat
  , isFloating =? True -?> doFloat
  , definiteToMaybe $ do
      minimized <- isMinimized
      trace $ "managehook: default placement (minimized: " ++ show minimized ++ ")"
      smartPlaceHook (30,30,30,30) (0.5,0.5)
  ] where
    definiteToMaybe = fmap Just -- inverse of X.H.ManageHelpers.maybeToDefinite
    smartPlaceHook gaps pos = placeHook (withGaps gaps (smart pos))
    anyWindowCurWS f = liftX $ or <$> (getStack >>= mapM (runQuery f) . W.integrate')

myLayout :: _ Window
myLayout =
    minimize
  . boringWindows
  -- . draggingVisualizer -- TODO breaks dragWindow action somehow
  . reduceBorders
  . MultiToggle.mkToggle1 NBFULL -- NOTE: This replaces the layout, including modifiers applied before it.
  . FS.fullscreenFull -- Fullscreen _NET_WM_STATE_FULLSCREEN layout support.
  . ManageDocks.avoidStruts -- NOTE: Apply avoidStruts late so that other modifiers aren't affected.
  . maximizeWithPadding 90 -- maximize overrides magnifier
  . magnify
  -- . mySpacing 1 2
  . windowNavigation
  $ toggledMods switchedLayouts
  where
    switchedLayouts = bsp ||| tmsWithGrid ||| grid ||| threeColMid ||| oneBig ||| tall ||| mouseResizable -- ||| full
    tmsWithGrid = TMC.tmsCombineTwo True 1 (3/100) (10/16) (TMC.RowsOrColumns True) grid -- (TMC.RowsOrColumns False)
    bsp = BSP.emptyBSP
    --tpp = TPP.TwoPanePersistent Nothing (3/100) (1/2)
    tall = Tall 1 (3/100) (1/2)
    grid = reflectHoriz (Grid (16/9))
    oneBig = OneBig (2/3) (2/3)
    threeColMid = ThreeColMid 1 (1/30) (4/9)

    --splitGrid  = SplitGrid GridV.T 1 2 (11/18) (4/3) (5/100)
    -- testing: this full + smartBorders instead of custom predicate
    full = Full -- NoBorders.noBorders (FS.fullscreenFull Full)
    mouseResizable = MRT.mouseResizableTileMirrored -- NOTE TODO: mirror modifier fails for this because mouse; could tip-toe around it with MRT.isMirrored maybe
      { MRT.nmaster    = 2
      , MRT.masterFrac = 50%100
      , MRT.slaveFrac  = 50%100 }

    reduceBorders = NoBorders.lessBorders NoBorders.Screen
    -- XXX: are these equivalent?
    --reduceBorders = NoBorders.lessBorders MyAmbiguity

    magnify = Magnifier.magnifyTop 1.3 (Magnifier.NoMaster 4) False

    -- NOTE: WindowNavigation interacts badly with some modifiers like "maximize" and "spacing", apply those after it.
    --
    -- NOTE: WindowNavigation spams lots of errors like this:
    --       "xmonad: X11 error: BadValue (integer parameter out of range for operation), request code=91, error code=2"
    -- since a recent change in xmonad core, without certain patch to xmonad-contrib (TODO: send upstream)
    --
    windowNavigation = WindowNavigation.configurableNavigation (WindowNavigation.navigateColor colBase00)

    toggledMods =
        MultiToggle.mkToggle1 HINT
      . MultiToggle.mkToggle1 NOBORDERS
      . MultiToggle.mkToggle1 REFLECTX -- NOTE: MIRROR with REFLECTX/Y is most intuitive when mirror goes first.
      . MultiToggle.mkToggle1 REFLECTY
      . MultiToggle.mkToggle1 MIRROR

    mySpacing :: Integer -> Integer -> _
    mySpacing sd wd = spacingRaw True (f sd) True (f wd) True
      where f n = Border n n n n

-- * Command bindings

myCmds :: (LayoutClass l Window, Read (l Window)) => CF.Cmd l ()
myCmds = CF.hinted "Commands" $ \helpCmd -> do

  let
      toggle1 a = Toggle' a

      skeys        = zip screenKeys [PScreen.P 0 ..]
      tags         = zip tagKeys [(0::Int)..]
      tagKeys      = map (:[]) ['a'..'z']
      screenKeys   = map (:[]) "wvz"

      pactl args = spawnProg "pactl" args ? unwords ("[PULSE]":args)
      mpc cmd    = spawn "mpc" [cmd] ? printf "MPD: %s" cmd
      clipmenu   = spawn "clipmenu" ["-p", "clipmenu", "-i"] ? "clipmenu"

      toggleMuteSource = pactl ["set-source-mute", "@DEFAULT_SOURCE@", "toggle"]
      toggleMuteSink   = pactl ["set-sink-mute", "@DEFAULT_SINK@", "toggle"]
      volume :: Int -> _
      volume d         = pactl ["set-sink-volume", "@DEFAULT_SINK@", printf "%+i%%" d]
      --mic    d         = pactl ["set-source-volume", "@DEFAULT_SOURCE@", printf "%+i%%" d]

      backlight :: Int -> _
      backlight d = spawn "xbacklight" [if d >= 0 then "-inc" else "-dec", printf "%i" (abs d)] ? printf "Backlight %+i%%" d

  group "Mouse" $ CF.modDef $ \modm -> do
    -- Kensington expert mouse button positions:
    --   2(TL) 8(TR)
    --     4(SCRU)
    --     5(SCRD)
    --   1(BL) 3(TR)
    --
    -- Button assignments: 1: left, 2: middle, 3: right, 4-5: scroll, 8: previous
    --
    let
      -- tiled:
      --   click: focus & move to master position
      --   drag:  swap with other tiled windows
      -- floating:
      --   click: sink window
      --   drag: change location of floating window
      action1 w = do
        isF <- runQuery isFloating w
        if isF then Flex.mouseWindow Flex.discrete w >> ifClick (windows $ W.sink w)
               else dragWindow w >> ifClick (windows $ W.focusWindow w >> W.swapMaster)

    (modm,               button1) /+ cmdT @"Drag tiled window"               . action1
    (modm,               button2) /+ cmdT @"Click on window swaps to master" . windows . (\w -> W.focusWindow w >> W.swapMaster)
    (modm,               button3) /+ cmdT @"flexible move window (discrete)" . Flex.mouseWindow Flex.discrete
    (modm .|. shiftMask, button3) /+ cmdT @"flexible resize window"          . Flex.mouseWindow Flex.resize

  group "XMonad & X11" $ do
    "M-<F1>" `CF.key'` helpCmd
    -- Terminal
    "M-<Return>"   >+ spawnTerm def "" ? "Terminal"
    "M-S-<Return>" >+ FloatNext.floatNext True >> spawnTerm def "" ? "Terminal (floating)"
    -- TODO use constructs like this instead of WindowCmd etc. sum types.
    "M-S-c"      >+ cmdT @"Kill (1 copy) window (X.A.CopyWindow)" CW.kill1
    "M-r M-S-c"  >+ cmdT @"Signal process (SIGKILL) of focused window (_NET_WM_PID)" (withFocused (signalProcessBy Posix.sigKILL))
    "M-$"        >+ -- spawn (sh "physlock -p \"${HOSTNAME} ${DISPLAY}\"") ? "Lock (physlock)"
     spawn (sh "dbus-send --type=method_call --dest=org.gnome.ScreenSaver /org/gnome/ScreenSaver org.gnome.ScreenSaver.Lock") ? "Lock the screen"
    "M-<Esc>"    >+ MyDebug.DebugStackSet
    "M-q"        >+ myRecompileRestart False True ? "Recompile && Restart"
    "M-C-q"      >+ myRecompileRestart True False ? "Recompile (force)"
    "M-S-q"      >+ io exitSuccess ? "Exit"
    "M-<Print>"  >+ takeScreenshot

  group "Prompts (XMonad)" $ do
    let allCommands =
          [ enumCommands (Proxy :: Proxy MiscCommand)
          , enumCommands (Proxy :: Proxy LayoutCommand)
          , enumCommands (Proxy :: Proxy LayoutGridCommand)
          ]
        cmdPromptAll = do
          bound <- boundCommands
          mkCmdPrompt (CmdPrompt "Cmd (ALL)" (nubBy ((==) `on` describe) $ concat allCommands ++ bound)) xpConfig
    "M-r M-c" >+ cmdPromptAll ? "Prompt: Cmd (ALL)"
    "M-r M-w" >+ cmdPrompt xpConfig (Proxy :: Proxy MiscCommand)
    "M-r M-l" >+ cmdPrompt xpConfig (Proxy :: Proxy LayoutCommand)
    "M-r M-g" >+ cmdPrompt xpConfig (Proxy :: Proxy LayoutGridCommand)
    "M-r S-w" >+ windowPromptWithMinimized xpConfig ? "Maximize Hidden Window (Prompt)"

  group "Execute" $ do
    "M-r r"            >+ spawnPrompt xpConfig "Execute"           ((\c -> spawnUnit def $ program (head c) (tail c)) . words)  ? "Execute (Prompt)"
    "M-r S-r"          >+ spawnPrompt xpConfig "Execute (T)"       ((\c -> spawnDialog' $ program (head c) (tail c)) . words) ? "Execute (Prompt)"
    "M-r <Return>"     >+ spawnPrompt xpConfig "Execute shell"     (spawnUnit def . shell)  ? "Execute shell (Prompt)"
    "M-r S-<Return>"   >+ spawnPrompt xpConfig "Execute shell (T)" (spawnDialog' . shell) ? "Execute shell in terminal (Prompt)"
    "M-r c"            >+ clipmenu
    "M-r b"            >+ spawnDialog "bluetoothctl" ? "bluetoothctl"
    "M-r m"            >+ spawn "xmag" ["-mag","2","-source","960x540"] ? "xmag"
    "M-r t"            >+ tmux Nothing ? "Terminal (tmux)"
    "M-r w"            >+ spawnDialog "scripts/fzf-wiki" ? "vimwiki (fzf)"

  group "Prompts (Execute)" $ do
    "M-r e"   >+ environPrompt xpConfig               ? "Environ (Prompt)"
    "M-r p"   >+ XP.Pass.passPrompt xpConfig          ? "Pass (Prompt)"
    "M-r C-p" >+ XP.Pass.passOTPPrompt xpConfig       ? "Pass OTP (Prompt)"
    "M-r C-u" >+ XP.Pass.passPromptWith "show-field --clip username" xpConfig ? "Pass username (Prompt)"
    "M-r q"   >+ XP.QB.qutebrowserP xpConfigNoHist "qutebrowser" ?+ XP.QB.qutebrowser ? "Prompt: qutebrowser"
    "M-r d"   >+ desktopEntryPrompt xpConfig [] ? "Desktop Entry Launch Prompt"
    "M-r s"   >+ inputPromptWithCompl xpConfig "scratchpad" (scratchpadCompl xpConfig myScratchpads) ?+ getAction . togglePad ? "Prompt: pad"
    "M-r u"   >+ inputPromptWithHistCompl xpConfig "browser-app" ?+ (\s -> launchDesktopEntry "chrome-app" [s]) ? "Chrome App"

  group "Media" $ do
    "M-+"                     >+ volume 3
    "M--"                     >+ volume (-3)
    "M-#"                     >+ togglePad "ncmpcpp"
    "M-c m"                   >+ spawnDialog "pulsemixer" ? "pulsemixer"
    "M-c n"                   >+ mpc "next"
    "M-c p"                   >+ mpc "prev"
    "M-c t"                   >+ mpc "toggle"
    "M-c y"                   >+ mpc "single"
    "M-c r"                   >+ mpc "random"
    "M-c s"                   >+ spawn "sink-switch" ? "Toggle speakers-phones output [PA]" -- uses a custom script in ~/bin
    "M-c P"                   >+ spawnDialog "scripts/fzf-mpc" ? "Browse playlists (FZF)"
    "<XF86AudioPlay>"         >+ mpc "toggle"
    "<XF86AudioStop>"         >+ mpc "stop"
    "<XF86AudioPrev>"         >+ mpc "prev"
    "<XF86AudioNext>"         >+ mpc "next"
    "<XF86AudioMute>"         >+ toggleMuteSink
    "<XF86AudioMicMute>"      >+ toggleMuteSource
    "<XF86AudioRaiseVolume>"  >+ volume 3
    "<XF86AudioLowerVolume>"  >+ volume (-3)
    "<XF86MonBrightnessUp>"   >+ backlight   2
    "<XF86MonBrightnessDown>" >+ backlight (-2)

  group "Layout" $ do
    "M-C-<Space>" >+ ResetLayout
    "M-<Space>"   >+ msgT NextLayout
    "M-S-<Space>" >+ msgT FirstLayout
    "M-x"         >+ msgT Shrink
    "M-S-x"       >+ msgT Expand
    "M-."         >+ msgT (IncMasterN 1)    :>> Mosaic.Taller
    "M-,"         >+ msgT (IncMasterN (-1)) :>> Mosaic.Wider
    "M-b t"       >+ msgT ManageDocks.ToggleStruts
    "M-b l"       >+ msgT Magnifier.Toggle
    "M-m"         >+ MaximizeRestore
    "M-b s"       >+ ToggleScreenSpacing :>> ToggleWindowSpacing
    "M-b b"       >+ toggle1 NOBORDERS
    "M-b h"       >+ toggle1 HINT
    "M-b f"       >+ toggle1 NBFULL
    "M-b m"       >+ toggle1 MIRROR
    "M-b x"       >+ toggle1 REFLECTX
    "M-b y"       >+ toggle1 REFLECTY
    "M-b M-x"     >+ msgT ManageDocks.ToggleStruts :>> toggle1 NOBORDERS :>> ToggleScreenSpacing :>> ToggleWindowSpacing

  group "Layout: BSP" $ do
    "M-C-y"   >+ msgT BSP.SelectNode
    "M-C-p"   >+ msgT BSP.MoveNode
    "M-C-u"   >+ msgT BSP.FocusParent
    "M-C-r"   >+ msgT BSP.Rotate
    "M-C-="   >+ msgT BSP.Equalize
    "M-C-!"   >+ msgT BSP.Balance
    "M-C-"    >>+ directions2D >++> msgT . BSP.ExpandTowards
    "M-r M-b" >+ cmdPrompt xpConfig (Proxy :: Proxy LayoutBSPCommand)

  group "Window" $ do
    "M-<Tab>"   >+ cyclePads
    "M-!"       >+ togglePad "tmux-0"
    "M-/"       >+ togglePad "dynamic"
    "M-"        >>+ directions2D >++> WindowNavigation.Go
    "M-S-"      >>+ directions2D >++> WindowNavigation.Swap
    "M-f "      >>+ directions2D >++> flip SnapMove   Nothing
    "M-f S-"    >>+ directions2D >++> flip SnapGrow   Nothing
    "M-f C-"    >>+ directions2D >++> flip SnapShrink Nothing
    "M-f ,"     >+ RotSlavesDown
    "M-f ."     >+ RotSlavesUp
    "M-f M-,"   >+ RotAllDown
    "M-f M-."   >+ RotAllUp
    "M-f M-m"   >+ FocusMaster
    "M-f M-n"   >+ FocusUp
    "M-f M-p"   >+ FocusDown
    "M-f m"     >+ SwapMaster
    "M-f n"     >+ SwapUp
    "M-f p"     >+ SwapDown
    "M-f u"     >+ FocusUrgent
    "M-f b"     >+ ToggleFocusedWindowBorder
    "M-f c"     >+ CenterWindow
    "M-f s"     >+ SinkWindow
    "M-f S-s"   >+ SinkAll
    "M-f f"     >+ FloatWindow
    "M-f S-f"   >+ ToggleFloatAllNew
    "M-f y"     >+ SwitchLayer
    "M-f h"     >+ pidPrompt xpConfig "SpawnOn/PPID" ?+ (\p -> wsPromptWithCurrent xpConfig "Shift to:" ?+ setManageByPPID p) ? "SpawnOn by Window PID"

  group "Screen" $ do
    "M-C-<Right>" >+ FocusScreenIn Prev
    "M-C-<Left>"  >+ FocusScreenIn Next
    let wsViewScreen scomp = cmdT1 @"View screen %s" (PScreen.viewScreen scomp) (printf "%i" . fromEnum)
    "M-"          >>+ skeys >++> wsViewScreen def
    "M-S-"        >>+ skeys >++> WorkspaceSendToScreen
    "M-M1-"       >>+ skeys >++> WorkspaceOnScreen FocusCurrent
    "M-C-"        >>+ skeys >++> WorkspaceOnScreen FocusNew

  group "Workspaces" $ do
    "M-; "        >>+ tags >++> WorkspaceView
    "M-; M-"      >>+ tags >++> WorkspaceCopy
    "M-S-; "      >>+ tags >++> WorkspaceShiftTo
    "M-y"         >+ WorkspaceCycleRecentHidden
    "M-S-n"       >+ WorkspaceSwapTo Next CycleWS.anyWS
    "M-S-p"       >+ WorkspaceSwapTo Prev CycleWS.anyWS
    "M-g r"       >+ WorkspaceSetNamePrompt
    "M-g n"       >+ WorkspaceAddPrompt
    "M-g d"       >+ WorkspaceRemoveFocused
    "M-g S-n"     >+ wsPromptNew' "New tag for window: " ?+ (\to -> DynWS.addHiddenWorkspace to >> defile (shift to))   ? "Move window to new tag (XP)"
    "M-g c"       >+ wsPrompt'    "Copy to tag: "        ?+ (\to -> withFocii $ \_ w -> windows $ CW.copyWindow w to)   ? "Copy window to this tag (XP)"
    "M-g m"       >+ wsPrompt'    "Shift to tag: "       ?+ (defile . shift                                           ) ? "Move window to this tag (XP)"
    "M-g g"       >+ wsPrompt'    "View tag: "           ?+ (defile . greedyView                                      ) ? "Go to tag (XP)"
    "M-g s"       >+ GS.goToSelected gsconfig1                          ? "Go to window (GS)"
    "M-g f"       >+ XP.windowPrompt xpConfigAuto XP.Goto XP.allWindows ? "Go to window (XP)"

  group "Workspace Groups" $ do
    "M-g M-n" >+ wsPromptNew' "New Workspace group: " ?+ addWSG ? "New WSG"
    "M-g M-g" >+ WSG.promptWSGroupView xpConfig "View WSG: " ? "View WSG (XP)"

  where
    directions2D = map (:[]) "kjlh" `zip` [minBound..maxBound @Direction2D]

    button8 :: Button
    button8 = 8

    takeScreenshot = spawn "scrot"
      [ "-u", "scrot_%Y-%d-%m_%H:%M.png"
      , "-e", "notify-send -i \"\\$(realpath $f)\" 'New screenshot' 'Name: $f\nWxH: $wx$h\nSize: $s bytes'" -- "mv -v -n -t ~/Pictures/ -- $f"
      ] ? "Take screenshot of focused window (~/)"

-- * Scratchpads

myScratchpads :: [Scratchpad]
myScratchpads =
    exclusive
    [ mkPad "tmux-0"     mhd  (appName =? "tmux-0")  (tmux (Just "0"))
    , mkPad "ncmpcpp"    mhd  (appName =? "ncmpcpp") (spawnTerm def{terminalName = "ncmpcpp"} "ncmpcpp")
    ] ++ [mkPadDyn "dynamic" xpConfig idHook]
  where
    mhd  = doRFRR 0.2 0.1 0.6 0.6
    --mhd' = doRFRR 0.2 0.1 0.7 0.7 -- TODO
    doRFRR x y w h = doRectFloat (W.RationalRect x y w h)

-- * Wallpapers

myWallpapers :: XConfig' l
myWallpapers = applyC $ \xc -> xc
  { logHook = logHook xc <+> setter }
  where
    setter = do
      tags <- gets (map W.tag . W.workspaces . windowset)
      wallpaperSetter (myWallpaperConf tags)

    myWallpaperConf tags = def
      { wallpaperBaseDir = "Pictures/Wallpaper"
      , wallpapers       = WallpaperList [(ws,WallpaperDir "3840x2160") | ws <- tags]
      }

-- * Actions

spawnDialog, spawnDialog' :: HasCmd X cmd => cmd -> X ()
spawnDialog  = spawnTerm def{ terminalName = "term-dialog", terminalGeometry = "130x40" }
spawnDialog' = spawnTerm def{ terminalName = "term-dialog", terminalGeometry = "130x40", terminalHold = True }

-- | Attach tmux session (or create one) in a new terminal window.
tmux :: Maybe String -> X ()
tmux msession = spawnTerm def{ terminalName = maybe "" ("tmux-"++) msession, terminalSaveLines = 0 } $
    program "tmux" $ ["new-session", "-A"] ++ maybe [] (\sname -> ["-s", sname]) msession

-- | Modified from XMonad.Main.handle
myRestartEventHook :: Event -> X All
myRestartEventHook e@ClientMessageEvent { ev_message_type = mt } = whenM' (fmap (mt ==) (getAtom "XMONAD_RESTART")) (myRestart >> mempty)
myRestartEventHook _                                             = mempty

myRecompileRestart :: Bool -> Bool -> X ()
myRecompileRestart rcFlag rsFlag = do
  saveWorkspaces
  dirs <- io getDirectories
  dir <- asks (dataDir . directories)
  prog <- io System.Environment.getProgName
  void $ userCode $ Notify.notifyLastS "Recompiling..."
  _p <- xfork $ whenM' (recompile dirs rcFlag) $ when' rsFlag $
    spawn $ program (dir </> prog) ["--restart"]
  return ()

myRestart :: X ()
myRestart = do
  dir  <- asks (dataDir . directories)
  prog <- io System.Environment.getProgName
  let msg = printf "Restart (%s)..." prog
  trace msg
  void $ userCode $ Notify.notifyLastS msg
  Notify.exitHook
  MyXmobar.exitHook
  restart (dir </> prog) True

-- Modified to not fire on spammy property updates (e.g. status bar stuff).
myUpdatePointer :: _ -> _ -> X ()
myUpdatePointer x y = whenX (check <$> asks currentEvent) (A.updatePointer x y)
  where check (Just PropertyEvent{}) = False
        check _                      = True

maximizeRestore' :: Window -> X ()
maximizeRestore' w =
  runQuery isFullscreen w >>= \case
    True  -> unFullscreen w
    False -> runQuery isFloating w >>= \case
      True  -> windows (W.sink w)
      False -> sendMessage (maximizeRestore w)

unFullscreen :: Window -> X ()
unFullscreen w = do
  a_st <- getAtom "_NET_WM_STATE"
  a_fs <- getAtom "_NET_WM_STATE_FULLSCREEN"
  wstate <- fromMaybe [] <$> WinProp.getProp32 a_st w
  withDisplay $ \dpy ->
    io $ changeProperty32 dpy w a_st aTOM propModeReplace (L.delete (fromIntegral a_fs) wstate)
  broadcastMessage (FS.RemoveFullscreen w)
  sendMessage FS.FullscreenChanged

centerOnScreen' :: Window -> X ()
centerOnScreen' win =
  curScreen >>= \W.Screen{screenDetail=SD sr@(Rectangle x y w h)} ->
  runQuery isFloating win >>= \isf ->
  withDisplay $ \dpy ->
  io (getWindowAttributes dpy win) >>= \WindowAttributes{..} ->
    let wr  = Rectangle (fromIntegral wa_x) (fromIntegral wa_y) (fromIntegral wa_width) (fromIntegral wa_height)
        ((a,b),(c,d)) = (Rect.center sr, Rect.center wr)
     in if isf && abs (c-a) + abs (d-b) < 5
           then windows (W.sink win) -- reset
           else FloatKeys.keysMoveWindowTo (x+round (w%2), y+round (h%2)) (0.5,0.5) win

myShowKeys :: CF.ShowKeys
myShowKeys ts km = do
  void $ userCode $
    Notify.notify_
    $ Notify.summary (Notify.raw $ rsummary ts)
    $ Notify.body (Notify.styleTT $ Notify.str textKm) def
  trace $ "Keys: " ++ rsummary ts ++ "\n" ++ textKm
  where
    rsummary []       = "<TOP>"
    rsummary (k:xs)   = "<" ++ k ++ "> " ++ intercalate ", " xs
    textKm = unlines (showKm km)

removeNoVisibleWS :: X ()
removeNoVisibleWS =
  curWorkspace >>= \ws ->
    whenX (and <$> mapM (runQuery isMinimized) (W.integrate' $ W.stack ws)) (remove ws)
  where
    remove ws = do
      DynWS.removeWorkspaceByTag (W.tag ws)
      DO.removeName (W.tag ws)
      saveWorkspaces

swapTo :: Direction1D -> WSType -> X ()
swapTo dir wsType = DO.swapWith dir wsType >> saveWorkspaces

addWorkspace :: String -> X ()
addWorkspace nm = DynWS.addWorkspace nm >> saveWorkspaces

addWSG :: String -> X ()
addWSG wsgId = do
  screens <- gets (W.screens . windowset)
  -- create workspaces
  wss <- forM screens $ \screen -> do
    let wsId = wsgId ++ show (toInteger (W.screen screen) + 1)
    addWorkspace wsId
    return (W.screen screen, wsId)
  -- create wsg
  WSG.addRawWSGroup wsgId wss
  -- view it
  WSG.viewWSGroup wsgId

renameWorkspace :: String -> X ()
renameWorkspace name = do
  cur <- curTag
  DO.updateName cur name
  DynWS.renameWorkspaceByName name
  saveWorkspaces

cycleRecentHiddenWS :: [KeySym] -> KeySym -> KeySym -> X ()
cycleRecentHiddenWS =
  cycleWindowSets $ \wset ->
    [W.tag ws | ws <- W.hidden wset ++ [W.workspace (W.current wset)]]

signalProcessBy :: Posix.Signal -> Window -> X ()
signalProcessBy s w = runQuery pid w ?+ \p ->
  confirmPrompt xpConfig (printf "kill -%i %i" (toInteger s) (toInteger p)) (io $ Posix.signalProcess s p)

myToggleFloatAllNew :: X ()
myToggleFloatAllNew = do
  FloatNext.toggleFloatAllNew
  next <- FloatNext.willFloatNext
  new <- FloatNext.willFloatAllNew
  void $ userCode $ Notify.notifyLastS $ if new
    then "Float hook: float all new windows"
    else if next
    then "Float hook: float next window"
    else "Float hook: inactive"

-- * Prompts

-- ** Workspace Prompts

-- | An existing WorkspaceId
wsPrompt' :: String -> X (Maybe WorkspaceId)
wsPrompt' pstr = (>>= either (const Nothing) Just) <$> wsPromptWith xpConfig pstr

wsPromptWithCurrent :: XP.XPConfig -> String -> X (Maybe WorkspaceId)
wsPromptWithCurrent xpconfig pstr = curTag >>= \tag ->
  (>>= either (const Nothing) Just) <$> wsPromptWith xpconfig{XP.defaultText=tag} pstr

-- | Non-existing Workspace name
wsPromptNew' :: String -> X (Maybe String)
wsPromptNew' pstr = (>>= either Just (const Nothing)) <$> wsPromptWith xpConfig pstr

-- | An existing WorkspaceId (Right) or some other string (Left).
wsPromptWith :: XP.XPConfig -> String -> X (Maybe (Either String WorkspaceId))
wsPromptWith xpconfig pstr = do
  ids <- gets (map W.tag . W.workspaces . windowset)
  XP.mkXPromptWithReturn (XP.Wor pstr) xpconfig
      (\s -> return $ filter (XP.searchPredicate xpConfig s) ids)
      (\r -> return $ maybe (Left r) Right (L.find (== r) ids))

-- ** Spawn Prompts

newtype SpawnPrompt = SpawnPrompt String

instance XP.XPrompt SpawnPrompt where
  showXPrompt (SpawnPrompt name) = name ++ ": "
  nextCompletion _ = XP.getNextCompletion
  commandToComplete _ = id

spawnPrompt :: XP.XPConfig -> String -> (String -> X ()) -> X ()
spawnPrompt xpconfig name go = do
  cmds <- io XP.Shell.getCommands
  let cfShell = XP.Shell.getShellCompl cmds (XP.searchPredicate xpconfig)
  cfHist <- XP.historyCompletionP (== name ++ ": ")
  XP.mkXPrompt (SpawnPrompt name) xpconfig (\s -> liftA2 (++) (cfHist s) (cfShell s)) go

-- ** Window Prompts

data WindowMinimizedPrompt = WindowMinimizedPrompt
instance XP.XPrompt WindowMinimizedPrompt where
  showXPrompt _ = "Window/Minimized> "
  nextCompletion _ = XP.getNextCompletion
  commandToComplete _ = id

windowPromptWithMinimized :: XP.XPConfig -> X ()
windowPromptWithMinimized xpconfig =
  XMonad.Actions.Minimize.withMinimized $ \ws ->
    mapM (runQuery title) ws >>= \compTxt ->
      XP.mkXPrompt WindowMinimizedPrompt xpconfig (XP.mkComplFunFromList' xpconfig compTxt)
        (\s -> mapM_ XMonad.Actions.Minimize.maximizeWindow (lookup s (zip compTxt ws)))

-- ** Utility

inputPromptWithHistCompl :: XP.XPConfig -> String -> X (Maybe String)
inputPromptWithHistCompl xpc name =
  XP.Input.inputPromptWithCompl xpc name =<< XP.historyCompletionP (== name ++ ": ")


-- * Command Groups as Types

type MiscCommand = "Misc." :??
  '[ WindowCmd
   , WindowNavigation.Navigate
   , MyFloatCmd
   , ToggleHookCmd
   , MyDebug.DebugCmd
   ]

type LayoutCommand = "Layout" :??
  '[ SetLayoutCmd
   , SendMessage ChangeLayout
   , SendMessage Resize
   , SendMessage IncMasterN
   , SendMessage ManageDocks.ToggleStruts
   , SendMessage Magnifier.MagnifyMsg
   , Mosaic.Aspect
   , Toggle' StdTransformers Window
   , Toggle' HINT Window
   , Toggle' REFLECTX Window
   , Toggle' REFLECTY Window
   --, WA.WindowArrangerMsg
   , LayoutBSPCommand
   ]

type LayoutBSPCommand = "BSP" :??
  '[ SendMessage BSP.TreeRotate
   , SendMessage BSP.TreeBalance
   , SendMessage BSP.ResizeDirectional
   , SendMessage BSP.Rotate
   , SendMessage BSP.Swap
   , SendMessage BSP.FocusParent
   , SendMessage BSP.SelectMoveNode
   , SendMessage BSP.SplitShiftDirectional
   ]

type LayoutGridCommand = "Grid" :??
  '[ ChangeGridGeom
   , ChangeMasterGridGeom
   , SendMessage Resize
   ]

-- * Command data-types

data SetLayoutCmd = ResetLayout
                  | MaximizeRestore
                  | ToggleScreenSpacing
                  | ToggleWindowSpacing
                  deriving (Show, Data)

data MyFloatCmd = CenterWindow
                | SinkWindow
                | SinkAll
                | FloatWindow
                | SwitchLayer
                | SnapMove   Direction2D (Maybe Int)
                | SnapGrow   Direction2D (Maybe Int)
                | SnapShrink Direction2D (Maybe Int)
                | PlaceSimpleSmart
                | ToggleFloatAllNew
                deriving (Show,Data)

data WindowCmd = FocusSwapMaster Window
               | FocusMaster
               | FocusUp
               | FocusDown
               | FocusUrgent
               | SwapMaster
               | SwapUp
               | SwapDown
               | RotSlavesDown
               | RotSlavesUp
               | RotAllDown
               | RotAllUp
               | ToggleFocusedWindowBorder
               | MaximizeWindow Window -- XMonad.Actions.Minimize
               | MinimizeWindow Window -- XMonad.Actions.Minimize
               | MinimizeFocused
               deriving (Show, Data)

-- XMonad.Hooks.ToggleHook
data ToggleHookCmd = ToggleHookAllNew String
                   | ToggleHookNext String
                   deriving (Show, Data)

data WorkspaceCmd = WorkspaceOnScreen Focus PScreen.PhysicalScreen -- greedyview
                  | WorkspaceView Int -- DynWS.withNthWorkspace / W.greedyView
                  | WorkspaceCopy Int -- DynWS.withNthWorkspace / CW.copy
                  | WorkspaceShiftTo Int -- DynWS.withNthWorkspace / W.shift
                  | WorkspaceCycleRecentHidden -- cycleRecentHiddenWS
                  | WorkspaceAddPrompt -- wsPromptNew' / DynWS.addWorkspace
                  | WorkspaceSetNamePrompt -- WSNames
                  | WorkspaceRemoveFocused              -- removeNoVisibleWS
                  | WorkspaceSwapTo Direction1D WSType  -- WSNames
                  | WorkspaceSendToScreen PScreen.PhysicalScreen
                  | WorkspaceViewScreen PScreen.PhysicalScreen
                  | FocusScreenIn Direction1D

-- * Instance Boilerplate (Cmd)

instance IsCmd ToggleHookCmd where
  cmdEnum _ = [ToggleHookAllNew h | h <- ["keepfocus"]]
  command (ToggleHookAllNew  hook) = ToggleHook.toggleHookAllNew hook ? printf "Toggle Hook %s (All)" hook
  command (ToggleHookNext hook)    = ToggleHook.toggleHookNext hook   ? printf "Toggle Hook %s (Next)" hook

instance IsCmd WorkspaceCmd where
  command (WorkspaceOnScreen focus' ps@(PScreen.P s')) = PScreen.getScreen    def ps ?+ (\s -> windows (W.currentTag >>= \x -> onScreen (W.greedyView x) focus' s)) ? printf "View screen %i (%s)" s' (show focus')
  command (WorkspaceSendToScreen   ps@(PScreen.P s')) = PScreen.sendToScreen def ps ? printf "Send workspace to screen %i" s'
  command (WorkspaceViewScreen     ps@(PScreen.P s')) = PScreen.viewScreen   def ps ? printf "View screen %i"              s'
  command (WorkspaceView    ws)  = DO.withNthWorkspace W.greedyView ws ? printf "View tag %i" ws
  command (WorkspaceCopy    ws)  = DO.withNthWorkspace CW.copy      ws ? printf "Copy focused to tag %i" ws
  command (WorkspaceShiftTo ws)  = DO.withNthWorkspace W.shift      ws ? printf "Move focused to tag %i" ws
  -- TODO keys
  command WorkspaceCycleRecentHidden = cycleRecentHiddenWS [xK_Super_L, xK_Alt_L] xK_y xK_p ? "Cycle (focus) recent tags"
  -- TODO DynWS + WSNames
  command WorkspaceAddPrompt     = wsPromptNew' "Add tag: " ?+ addWorkspace ? "New tag (XP)"
  command WorkspaceSetNamePrompt = wsPromptNew' "Rename tag: " ?+ renameWorkspace ? "Rename this tag (XP)"
  command WorkspaceRemoveFocused = removeNoVisibleWS ? "Remove this tag (if empty)"
  command (WorkspaceSwapTo d t)  = swapTo d t >> saveWorkspaces ? printf "Shift current tag %s" (if d == Next then "forward" else "backwards")
  command (FocusScreenIn Next)   = CycleWS.nextScreen ? "Focus next screen"
  command (FocusScreenIn Prev)   = CycleWS.prevScreen ? "Focus previous screen"

  cmdEnum _ = [WorkspaceAddPrompt,WorkspaceCycleRecentHidden,WorkspaceRemoveFocused]

instance IsCmd SetLayoutCmd where
  command ResetLayout         = (asks (layoutHook . config) >>= setLayout) ? "Reset layout"
  command ToggleScreenSpacing = toggleScreenSpacingEnabled                     ? "Toggle screen spacing"
  command ToggleWindowSpacing = toggleWindowSpacingEnabled                     ? "Toggle window spacing"
  command MaximizeRestore     = withFocused maximizeRestore'                   ? "Maximize / restore window"

instance IsCmd MyFloatCmd where
  command CenterWindow      = withFocused centerOnScreen'             ? "Place focused (center)"
  command SinkWindow        = withFocused (windows . W.sink)          ? "Sink focused"
  command SinkAll           = windows (\w -> foldr W.sink w (W.allWindows w))          ? "Sink all windows"
  command FloatWindow       = withFocused (\w -> windows . W.float w . snd =<< floatLocation w)         ? "Float focused"
  command SwitchLayer       = Navigation2D.switchLayer                ? "Switch Layer (Nav2D)"
  command PlaceSimpleSmart  = placeFocused simpleSmart                ? "Place focused (simpleSmart)"
  command ToggleFloatAllNew = myToggleFloatAllNew             ? "Float new windows (toggle)"
  command (SnapMove   d2 p) = withFocused (FloatSnap.snapMove   d2 p) ? printf "Move %s (FloatSnap)" (show d2)
  command (SnapGrow   d2 p) = withFocused (FloatSnap.snapGrow   d2 p) ? printf "Grow %s (FloatSnap)" (show d2)
  command (SnapShrink d2 p) = withFocused (FloatSnap.snapShrink d2 p) ? printf "Shrink %s (FloatSnap)" (show d2)

instance IsCmd WindowCmd where
  command FocusMaster               = BW.focusMaster                                            ? "Focus master (BoringWindows)"
  command FocusUp                   = BW.focusUp                                                ? "Focus up (BoringWindows)"
  command FocusDown                 = BW.focusDown                                              ? "Focus down (BoringWindows)"
  command FocusUrgent               = focusUrgent                                               ? "Focus urgent window"
  command SwapMaster                = windows W.swapMaster                                      ? "Swap to master"
  command SwapUp                    = windows W.swapUp                                          ? "Swap up"
  command SwapDown                  = windows W.swapDown                                        ? "Swap down"
  command RotSlavesDown             = RotSlaves.rotSlavesDown                                   ? "Rotate slaves down"
  command RotSlavesUp               = RotSlaves.rotSlavesUp                                     ? "Rotate slaves up"
  command RotAllDown                = RotSlaves.rotAllDown                                      ? "Rotate down"
  command RotAllUp                  = RotSlaves.rotAllUp                                        ? "Rotate up"
  command (FocusSwapMaster w)       = windows (W.focusWindow w >> W.swapMaster)   ? "Focus window & swap it master"
  command ToggleFocusedWindowBorder = (withFocused toggleBorder >> refresh) ? "Toggle focused window border"
  command (MaximizeWindow w)        = XMonad.Actions.Minimize.maximizeWindow w ? "Maximize window"
  command (MinimizeWindow w)        = XMonad.Actions.Minimize.minimizeWindow w ? "Minimize window"
  command MinimizeFocused           = withFocused XMonad.Actions.Minimize.minimizeWindow ? "Minimize Focused"

-- * Persistent workspaces

wsFile :: MonadIO m => m FilePath
wsFile = do
  dir <- cacheDir <$> io getDirectories
  return (dir </> "workspaces")

restoreWorkspaces :: XConfig' l
restoreWorkspaces = applyIO $ \xc -> readState <&> maybe xc (go xc)
  where
    readState :: IO (Maybe [(Int, String)])
    readState = do
      file <- wsFile
      whenM' (doesFileExist file) $ do
        contents <- readFile file
        case readMaybe contents of
          Nothing  -> trace "failed to read workspaces file" >> return Nothing
          Just wss -> return $ Just $ zip [0..] wss

    go xc wss = xc { workspaces = map snd wss }

saveWorkspaces :: X ()
saveWorkspaces = do
  wsSort <- DO.getSortByOrder
  tags <- gets (wsSort . W.workspaces . windowset)
  let names = [ W.tag t | t <- tags]
  file <- wsFile
  io $ writeFile file (show names)

  dir <- cacheDir <$> io getDirectories
  let statefile = dir </> "extState"
  extst <- gets (fmap f . extensibleState)
  io $ writeFile statefile (show extst)
    where
      f :: Either String StateExtension -> (String, String)
      f x@(Left s)                        = (show (typeOf x), s)
      f x@(Right StateExtension{})        = (show (typeOf x), "n/a")
      f x@(Right (PersistentExtension a)) = (show (typeOf x), show a)

-- * MyAmbiguity LayoutMod (NoBorders.SetsAmbiguous)

data MyAmbiguity = MyAmbiguity deriving (Read, Show)

instance NoBorders.SetsAmbiguous MyAmbiguity where
  hiddens MyAmbiguity{} wset lr _mst wrs = tiled wrs `L.union` floats
    where
      tiled [(w,_)] = [w]
      tiled xs      = [w | (w,r) <- xs, r == lr]
      floats =
        [w |
          W.Screen{workspace=wspace, screenDetail=SD sr} <- W.screens wset
        , w  <- W.integrate' $ W.stack wspace
        , wr <- maybeToList $ M.lookup w (W.floating wset)
        , sr == scaleRationalRect sr wr
        ]

-- * Configuration helpers

applyC :: (XConfig l -> XConfig l) -> XConfig' l
applyC f xc = xc <&> f

applyIO :: (XConfig l -> IO (XConfig l)) -> XConfig' l
applyIO f xc = xc >>= f

type XConfig' l = IO (XConfig l) -> IO (XConfig l)

foo = "bar"
