{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE DeriveDataTypeable        #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE LambdaCase                #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE PartialTypeSignatures     #-}
{-# LANGUAGE RebindableSyntax          #-}
{-# LANGUAGE RecordWildCards           #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TypeApplications          #-}
{-# LANGUAGE TypeOperators             #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}
-- misc
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}
-- For Prime
{-# OPTIONS_GHC -Wno-unused-local-binds -Wno-unused-do-bind #-}


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
--
-- https://specifications.freedesktop.org/wm-spec/wm-spec-latest.html#idm45207712345936
--
-- google-chrome-stable, chromium, etc. whatever with --app switch
--
--  file chrome-app.desktop:
--
--  [Desktop Entry]
--  Version=1.0
--  Encoding=UTF-8
--  Type=Application
--  Name=Chrome App Mode
--  Terminal=false
--  StartupWMClass=Google-chrome
--  StartupNotify=true
--  Exec=/usr/bin/google-chrome-stable --new-window --app=%u
--  Icon=google-chrome
--
--  desktop-file-install --dir=$XDG_DATA_HOME/applications chrome-app.desktop
------------------------------------------------------------------------------

module Main (main) where

import qualified XMonad                              as X
import qualified XMonad.StackSet                     as W

import qualified XMonad.Actions.CopyWindow           as CW
import           XMonad.Actions.CycleRecentWS        (cycleWindowSets)
import           XMonad.Actions.CycleWS              (WSType(..))
import qualified XMonad.Actions.CycleWS              as CycleWS
import qualified XMonad.Actions.DynamicWorkspaces    as DynWS
import qualified XMonad.Actions.FlexibleManipulate   as Flex
import qualified XMonad.Actions.FloatKeys            as FloatKeys
import qualified XMonad.Actions.FloatSnap            as FloatSnap
import qualified XMonad.Actions.GridSelect           as GS
import qualified XMonad.Actions.Minimize
import qualified XMonad.Actions.Navigation2D         as Navigation2D
import           XMonad.Actions.NoBorders            (toggleBorder)
import           XMonad.Actions.OnScreen             (Focus(..), onScreen)
import qualified XMonad.Actions.PhysicalScreens      as PScreen
import qualified XMonad.Actions.RotSlaves            as RotSlaves
import qualified XMonad.Actions.SpawnOn              as SpawnOn
import qualified XMonad.Actions.UpdatePointer        as A (updatePointer)
import qualified XMonad.Actions.WorkspaceNames       as WSNames
import           XMonad.Config.Prime                 hiding (spawn, (>>))
import qualified XMonad.Config.Prime                 as Arr ((>>))
import qualified XMonad.Hooks.EwmhDesktops           as EWMH
import           XMonad.Hooks.FadeWindows            (isFloating)
import qualified XMonad.Hooks.FloatNext              as FloatNext
import qualified XMonad.Hooks.InsertPosition         as InsertPosition
import qualified XMonad.Hooks.ManageDebug
import qualified XMonad.Hooks.ManageDocks            as ManageDocks
import           XMonad.Hooks.ManageHelpers
import           XMonad.Hooks.Minimize               (minimizeEventHook)
import           XMonad.Hooks.Place                  (placeFocused, placeHook, simpleSmart, smart, underMouse, withGaps)
import qualified XMonad.Hooks.ToggleHook             as ToggleHook
import           XMonad.Hooks.UrgencyHook            (focusUrgent)
import qualified XMonad.Hooks.UrgencyHook            as Urgency
import           XMonad.Hooks.WallpaperSetter
import qualified XMonad.Layout.MouseResizableTile    as MRT
import qualified XMonad.Layout.BinarySpacePartition  as BSP
import           XMonad.Layout.BoringWindows         (boringWindows)
import qualified XMonad.Layout.BoringWindows         as BW
import qualified XMonad.Layout.Fullscreen            as FS
import           XMonad.Layout.GridVariants          (ChangeGridGeom(..), ChangeMasterGridGeom(..), Grid(Grid), SplitGrid(..))
import qualified XMonad.Layout.GridVariants          as GridV (Orientation(..))
import qualified XMonad.Layout.LayoutHints           as LayoutHints
import           XMonad.Layout.Magnifier             (magnifierOff)
import qualified XMonad.Layout.Magnifier             as Magnifier
import           XMonad.Layout.Maximize              (maximizeRestore, maximizeWithPadding)
import           XMonad.Layout.Minimize              (minimize)
import qualified XMonad.Layout.Minimize
import qualified XMonad.Layout.Mosaic                as Mosaic
import qualified XMonad.Layout.MultiToggle           as MultiToggle
import           XMonad.Layout.MultiToggle.Instances
import qualified XMonad.Layout.NoBorders             as NoBorders
import           XMonad.Layout.OneBig                (OneBig(OneBig))
import           XMonad.Layout.Reflect               (REFLECTX(..), REFLECTY(..), reflectHoriz)
import           XMonad.Layout.Spacing               (Border(Border), spacingRaw, toggleScreenSpacingEnabled, toggleWindowSpacingEnabled)
import           XMonad.Layout.ThreeColumns          (ThreeCol(ThreeColMid))
import           XMonad.Layout.WindowNavigation      (Navigate(..), configurableNavigation, navigateColor)
import qualified XMonad.Prompt                       as XP
import           XMonad.Prompt.ConfirmPrompt         (confirmPrompt)
import           XMonad.Prompt.Input                 (inputPromptWithCompl, (?+))
import qualified XMonad.Prompt.Input                 as XP.Input
import qualified XMonad.Prompt.Pass                  as XP.Pass
import qualified XMonad.Prompt.Shell                 as XP.Shell
import qualified XMonad.Prompt.Window                as XP (WindowPrompt(Goto), allWindows, windowPrompt)
import qualified XMonad.Prompt.Workspace             as XP (Wor(Wor))
import           XMonad.Util.NamedActions            (showKm)
import qualified XMonad.Actions.AfterDrag            as AfterDrag
import           XMonad.Util.PureX
import qualified XMonad.Util.Rectangle               as Rect
import qualified XMonad.Util.StringProp              as StringProp
import           XMonad.Util.Types                   (Direction1D(..), Direction2D(..))
import qualified XMonad.Util.WindowProperties        as WinProp

import           Control.Applicative
import           Control.Monad                       hiding ((>>), (>>=))
import qualified Control.Monad
import qualified Data.List                           as L
import qualified Data.Map                            as M
import           Data.Maybe
import           Data.Monoid
import           Data.Proxy
import           Data.Ratio                          ((%))
import qualified System.Environment
import           System.Exit                         (exitSuccess)
import           System.Directory                    (doesFileExist)
import           System.FilePath                     ((</>))
import qualified System.Posix                        as Posix
import           Text.Printf                         (printf)
import           Text.Read                           (readMaybe)

import           DesktopEntries
import qualified MyDebug
import           MyRun
import           MyTheme
import qualified MyXmobar
import           Scratchpads
import           SpawnOnByPPID
import           XMonad.Config.CommandsKeysF
import qualified XMonad.Config.CommandsKeysF         as CF
import           XMonad.Hooks.EwmhDesktopsEx         (setEWMHDesktopGeometry)
import           XMonad.Layout.Hinted
import           XMonad.Prompt.Environ               (environPrompt)
import qualified XMonad.Prompt.Qutebrowser           as XP.QB
import qualified XMonad.Util.DesktopNotifications    as Notify
import           XMonad.Util.NamedCommands
import           XMonad.Util.NamedCommands.Orphans

import Data.Int (Int32)

-- * Main

main :: IO ()
main = xmonad configPrime

-- * Config

configPrime :: _ => Prime l _
configPrime = do
  terminal           =: "my-terminal"
  borderWidth        =: 1
  focusedBorderColor =: colCyan
  normalBorderColor  =: colBase02
  modMask            =: mod4Mask
  focusFollowsMouse  =: True
  clickJustFocuses   =: False
  clientMask         =+ focusChangeMask -- default: structureNotifyMask .|. enterWindowMask .|. propertyChangeMask@
  rootMask           =+ focusChangeMask -- default: substructureRedirectMask .|. substructureNotifyMask .|. enterWindowMask .|. leaveWindowMask .|. structureNotifyMask .|. buttonPressMask

  manageHook =: (MyDebug.myDebugManageHook <+> XMonad.Hooks.ManageDebug.maybeManageDebug)
  manageHook =+ myManageHook
  manageHook =+ ManageDocks.manageDocks
  manageHook =+ ToggleHook.toggleHook "keepfocus" (InsertPosition.insertPosition InsertPosition.Above InsertPosition.Older) -- default: Above Never
  manageHook =+ (FS.fullscreenManageHook <+> SpawnOn.manageSpawn <+> FloatNext.floatNextHook)

  myLayout

  apply $ MyXmobar.myStatusBars . ManageDocks.docks

  handleEventHook =+ docksEventHookExtra

  applyIO $ CF.addAll myShowKeys myCmds

  startupHook =+ (Notify.startupHook
    <+> restoreWorkspaces
    <+> setEWMHDesktopGeometry
    <+> scratchpadsStartupHook myScratchpads)

  handleEventHook =+ myRestartEventHook
  handleEventHook =+ MyDebug.debugEventHook

  handleEventHook =+ FS.fullscreenEventHook
  handleEventHook =+ minimizeEventHook    -- Handle minimize/maximize requests
  handleEventHook =+ LayoutHints.hintsEventHook       -- Refreshes the layout whenever a window changes its hints.

  logHook =+ XMonad.Hooks.ManageDebug.manageDebugLogHook
  --logHook =+ EWMH.ewmhDesktopsLogHookCustom id
  logHook =+ myUpdatePointer (0.5, 0.5) (0.4, 0.4)
  logHook =+ wallpaperSetter myWallpaperConf

  urgencyHook Notify.urgencyHook

  apply EWMH.ewmh
  where
    (>>) = (Arr.>>)

    urgencyHook :: (Urgency.UrgencyHook h, LayoutClass l Window) => h -> Prime l l
    urgencyHook = apply . flip Urgency.withUrgencyHookC Urgency.urgencyConfig { Urgency.suppressWhen = Urgency.Focused }

restoreWorkspaces :: X ()
restoreWorkspaces = do
  dir <- asks (cacheDir . directories)
  let file = dir </> "workspaces"
  io $ putStrLn $ "File is: " ++ file
  io (doesFileExist file) >>= \case
    True  -> do
      contents <- io (readFile file)
      case readMaybe contents of
        Nothing -> void $ trace "failed to read workspaces file"
        Just (names :: [(String, String)]) -> do
          getName <- WSNames.getWorkspaceNames'
          forM_ names $ \(tag, name) ->
            when (isNothing $ getName tag) $ WSNames.setWorkspaceName tag name
    False -> return ()
  where
    (>>) = (Control.Monad.>>)

-- TODO restore the number of workspaces too
-- restoreWorkspaces :: XConfig l -> IO (XConfig l)
-- restoreWorkspaces xc = do
--   dir <- cacheDir <$> getDirectories
--   let file = dir </> "workspaces"
--   putStrLn $ "File is: " ++ file
--   putStrLn $ "Current workspaces are: " ++ show (X.workspaces xc)
--   doesFileExist file >>= \case
--     True  -> do
--       readMaybe <$> readFile file >>= \case
--         Nothing -> trace "failed to read workspaces file" >> return xc
--         Just ws -> return xc { X.workspaces = ws }
--       return xc
--     False -> return xc
--   where
--     (>>) = (Control.Monad.>>)

saveWorkspaces :: X ()
saveWorkspaces = do
  dir <- asks (cacheDir . directories)
  let file = dir </> "workspaces"
  tags <- gets (W.workspaces . windowset)
  getName <- WSNames.getWorkspaceNames'
  let names = [ (W.tag t, fromMaybe (W.tag t) (getName $ W.tag t)) | t <- tags]
  io $ writeFile file (show names)

myScratchpads :: [Scratchpad]
myScratchpads =
    exclusive
    [ mkPad "tmux-0"     mhd  (appName =? "tmux-0")  (tmux (Just "0"))
    , mkPad "ncmpcpp"    mhd  (appName =? "ncmpcpp") (spawnTerm def{terminalName = "ncmpcpp"} "ncmpcpp")
    ] ++ [mkPadDyn "dynamic" xpConfig idHook]
  where
    mhd  = doRFRR 0.2 0.1 0.6 0.6
    mhd' = doRFRR 0.2 0.1 0.7 0.7 -- TODO
    doRFRR x y w h = doRectFloat (W.RationalRect x y w h)

myLayout :: LayoutClass l Window => Prime l _
myLayout = do
  -- Default Layout
  resetLayout BSP.emptyBSP
  --  Other layouts
  addLayout basicGrid
  addLayout MRT.mouseResizableTileMirrored -- NOTE: mirror modifier fails for this because mouse. TODO: could switch MRT.isMirrored
    { MRT.nmaster    = 2
    , MRT.masterFrac = 50%100
    , MRT.slaveFrac  = 50%100 }
  addLayout (OneBig (2/3) (2/3))
  addLayout (Tall 1 (3/100) (1/2))
  --addLayout splitGrid
  --addLayout (ThreeColMid 1 (1/30) (4/9))
  --addLayout (Mosaic.mosaic 1.25 [3,2])

  -- Modifiers
  -- NOTE: MIRROR with REFLECTX/Y is most intuitive when mirror goes first.
  modifyLayout (MultiToggle.mkToggle1 MIRROR)
  modifyLayout (MultiToggle.mkToggle1 REFLECTX)
  modifyLayout (MultiToggle.mkToggle1 REFLECTY)
  modifyLayout (MultiToggle.mkToggle1 NOBORDERS)
  modifyLayout (MultiToggle.mkToggle1 HINT)
  modifyLayout mySaneLayoutModifiers
  where
    (>>) = (Arr.>>)

    defaultGridRatio = 16/9
    basicGrid  = reflectHoriz (Grid defaultGridRatio)
    splitGrid  = SplitGrid GridV.T 1 2 (11/18) (4/3) (5/100)


mySpacing sd wd = spacingRaw True (f sd) True (f wd) True
  where f n = Border n n n n

-- Stack of common layout modifiers that are useful applied to any layout
mySaneLayoutModifiers :: _ -> _ Window
mySaneLayoutModifiers =
      minimize
    . boringWindows
    . NoBorders.lessBorders MyAmbiguity
    . MultiToggle.mkToggle1 NBFULL -- NOTE: This replaces the layout, including modifiers applied before it.
    . FS.fullscreenFull -- Fullscreen _NET_WM_STATE_FULLSCREEN layout support.
    . ManageDocks.avoidStruts -- NOTE: Apply avoidStruts late so that other modifiers aren't affected.
    . magnifierOff
    . maximizeWithPadding 80
    . mySpacing 1 2
    . configurableNavigation (navigateColor colBase00) -- NOTE: WindowNavigation interacts badly with some modifiers like "maximize" and "spacing", apply those after it.


myCmds :: (LayoutClass l Window, Read (l Window)) => CF.Cmd l ()
myCmds = CF.hinted "Commands" $ \helpCmd -> do

  let unPScreen (PScreen.P s) = s
      onPScreen f g a ps = PScreen.getScreen def ps ?+ \s -> windows (g >>= \x -> onScreen (f x) a s)

      toggle1 a = Toggle' a

      skeys        = zip screenKeys [PScreen.P 0 ..]
      tags         = zip tagKeys [(0::Int)..]
      tagKeys      = map (:[]) ['a'..'z']
      screenKeys   = map (:[]) "wvz"

      pactl args = spawnProg "pactl" args ? unwords ("[PULSE]":args)
      mpc cmd    = spawn "mpc" [cmd] ? printf "MPD: %s" cmd
      clipmenu   = spawn "clipmenu" ["-p", "clipmenu", "-i"] ? "clipmenu"

      volume, mic :: Int -> _
      toggleMuteSource = pactl ["set-source-mute", "@DEFAULT_SOURCE@", "toggle"]
      toggleMuteSink   = pactl ["set-sink-mute", "@DEFAULT_SINK@", "toggle"]
      volume d         = pactl ["set-sink-volume", "@DEFAULT_SINK@", printf "%+i%%" d]
      mic    d         = pactl ["set-source-volume", "@DEFAULT_SOURCE@", printf "%+i%%" d]

      backlight :: Int -> _
      backlight d = spawn "xbacklight" [if d >= 0 then "-inc" else "-dec", printf "%i" (abs d)] ? printf "Backlight %+i%%" d

  group "Mouse" $ CF.modDef $ \modm -> do
    (modm,               button1) /+ cmdT @"flexible move window (discrete)" . Flex.mouseWindow Flex.discrete
    (modm .|. shiftMask, button1) /+ cmdT @"flexible resize window"          . Flex.mouseWindow Flex.resize
    (modm,               button2) /+ cmdT @"Click on window swaps to master" . windows . (\w -> W.focusWindow w Control.Monad.>> W.swapMaster)
    --
    -- Button assignments: 1: left, 2: middle, 3: right, 4-5: scroll, 8: previous
    --
    -- Kensington mouse button order: 1: bottom left, 2: top left, 3: bottom right, 8: top right
    --
    -- TODO use stuff from AfterDrag.

  group "XMonad & X11" $ do
    "M-<F1>" `CF.key'` helpCmd
    -- Terminal
    "M-<Return>"   >+ spawnTerm def "" ? "Terminal"
    "M-S-<Return>" >+ FloatNext.floatNext True >> spawnTerm def "" ? "Terminal (floating)"
    -- TODO use constructs like this instead of WindowCmd etc. sum types.
    "M-S-c"      >+ cmdT @"Kill (1 copy) window (X.A.CopyWindow)" CW.kill1
    "M-r M-S-c"  >+ cmdT @"Signal process (SIGKILL) of focused window (_NET_WM_PID)" (withFocused (signalProcessBy Posix.sigKILL))

    "M-$"        >+ spawn (sh "physlock -p \"${HOSTNAME} ${DISPLAY}\"") ? "Lock (physlock)"
    "M-<Esc>"    >+ MyDebug.DebugStackSet
    "M-q"        >+ myRecompileRestart False True ? "Recompile && Restart"
    "M-C-q"      >+ myRecompileRestart True False ? "Recompile (force)"
    "M-S-q"      >+ io exitSuccess ? "Exit"
    "M-<Print>"  >+ takeScreenshot

  group "Prompts (XMonad)" $ do
    "M-r M-c" >+ promptCommand xpConfig
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
    "M-C-="   >+ msgT BSP.Equalize
    "M-C-!"   >+ msgT BSP.Balance
    "M-C-"    >>+ directions2D >++> msgT . BSP.ExpandTowards
    "M-r M-b" >+ cmdPrompt xpConfig (Proxy :: Proxy LayoutBSPCommand)

  group "Window" $ do
    "M-<Tab>"   >+ cyclePads
    "M-!"       >+ togglePad "tmux-0"
    "M-/"       >+ togglePad "dynamic"
    "M-"        >>+ directions2D >++> Go
    "M-S-"      >>+ directions2D >++> Swap
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

  where
    (>>) = (Control.Monad.>>)

    directions2D = map (:[]) "kjlh" `zip` [minBound..maxBound @Direction2D]

    button8 :: Button
    button8 = 8

    takeScreenshot = spawn "scrot"
      [ "-u", "scrot_%Y-%d-%m_%H:%M.png"
      , "-e", "notify-send -i \"\\$(realpath $f)\" 'New screenshot' 'Name: $f\nWxH: $wx$h\nSize: $s bytes'" -- "mv -v -n -t ~/Pictures/ -- $f"
      ] ? "Take screenshot of focused window (~/)"

myManageHook :: ManageHook
myManageHook = composeOne
  [ managePads
  , transience
  , appName   =? "term-dialog"     -?> doCenterFloat
  , className =? "feh"             -?> smartPlaceHook (16,5,16,5) (0,0) <+> doFloat
  , className =? "Xmessage"        -?> doCenterFloat
  , className =? "Xmag"            -?> doSideFloat NC
  , className =? "Nvidia-settings" -?> doCenterFloat
  , className =? "zoom" <&&> title /=? "Zoom Meeting" -?> doFloat
  , isDialog                       -?> placeHook (underMouse (0.7,0.7)) <+> doFloat
  , isFloating =? False <&&> anyWindowCurWS isFullscreen -?> doFloat
  , definiteToMaybe $ smartPlaceHook (30,30,30,30) (0.5,0.5)
  ] where
    definiteToMaybe = fmap Just -- inverse of X.H.ManageHelpers.maybeToDefinite
    smartPlaceHook gaps pos = placeHook (withGaps gaps (smart pos))
    anyWindowCurWS f = liftX $ or <$> (getStack >>= mapM (runQuery f) . W.integrate')

myWallpaperConf :: WallpaperConf
myWallpaperConf = def
  { wallpaperBaseDir = "Pictures/Wallpaper"
  , wallpapers       = WallpaperList [(ws,WallpaperDir "3840x2160") | ws <- show <$> [1..20::Int]]
  }

-- * Actions

-- | Modified from XMonad.Main.handle
myRestartEventHook :: Event -> X All
myRestartEventHook e@ClientMessageEvent { ev_message_type = mt } = whenM' (fmap (mt ==) (getAtom "XMONAD_RESTART")) (myRestart Control.Monad.>> mempty)
myRestartEventHook _                                             = mempty

myRecompileRestart :: Bool -> Bool -> X ()
myRecompileRestart rcFlag rsFlag = do
  saveWorkspaces
  statefilename <- asks (stateFileName . directories)
  dirs <- io getDirectories
  dir <- asks (dataDir . directories)
  prog <- io System.Environment.getProgName
  Notify.notifyLastS $ "Recompiling. State file is " ++ statefilename
  _p <- xfork $ whenM' (recompile dirs rcFlag) $ when' rsFlag $
    spawn $ program (dir </> prog) ["--restart"]
  return ()
  where
    (>>) = (Control.Monad.>>)

myRestart :: X ()
myRestart = do
  dir  <- asks (dataDir . directories)
  prog <- io System.Environment.getProgName
  let msg = printf "Restart (%s)..." prog
  trace msg
  Notify.notifyLastS msg
  Notify.exitHook
  MyXmobar.exitHook
  restart (dir </> prog) True
  where
    (>>) = (Control.Monad.>>)

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
    where
      (>>) = (Control.Monad.>>)

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
myShowKeys ts xs = do
  Notify.notify_ $ Notify.summary (unwords ("Keys":ts)) $ Notify.body ("<tt>" ++ unlines (showKm xs) ++ "</tt>") def
  trace $ "Keys: " ++ unwords ("Keys":ts) ++ unlines (showKm xs)
  where
    (>>) = (Control.Monad.>>)

removeNoVisibleWS :: X ()
removeNoVisibleWS =
  curWorkspace >>= \ws ->
    whenX (and <$> mapM (runQuery isMinimized) (W.integrate' $ W.stack ws)) DynWS.removeWorkspace

cycleRecentHiddenWS :: [KeySym] -> KeySym -> KeySym -> X ()
cycleRecentHiddenWS =
  cycleWindowSets $ \wset ->
    [W.tag ws | ws <- W.hidden wset ++ [W.workspace (W.current wset)]]

signalProcessBy :: Posix.Signal -> Window -> X ()
signalProcessBy s w = runQuery pid w ?+ \p ->
  confirmPrompt xpConfig (printf "kill -%i %i" (toInteger s) (toInteger p)) (io $ Posix.signalProcess s p)

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
  wsname' <- WSNames.getWorkspaceNames'
  XP.mkXPromptWithReturn (XP.Wor pstr) xpconfig
      (\s -> return $ filter (XP.searchPredicate xpConfig s) (mapMaybe wsname' ids))
      (\r -> return $ maybe (Left r) Right $ wsname' r)

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

-- * Programs

spawnDialog  = spawnTerm def{ terminalName = "term-dialog", terminalGeometry = "130x40" }
spawnDialog' = spawnTerm def{ terminalName = "term-dialog", terminalGeometry = "130x40", terminalHold = True }

-- | Attach tmux session (or create one) in a new terminal window.
tmux :: Maybe String -> X ()
tmux msession = spawnTerm def{ terminalName = maybe "" ("tmux-"++) msession, terminalSaveLines = 0 } $
    program "tmux" $ ["new-session", "-A"] ++ maybe [] (\sname -> ["-s", sname]) msession


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

-- * Command Groups as Types

type MiscCommand = "Misc." :??
  '[ WindowCmd
   , Navigate
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

-- * Custom Command data-types

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

-- * Instance Boilerplate

instance IsCmd ToggleHookCmd where
  cmdEnum _ = [ToggleHookAllNew h | h <- ["keepfocus"]]
  command (ToggleHookAllNew  hook) = ToggleHook.toggleHookAllNew hook ? printf "Toggle Hook %s (All)" hook
  command (ToggleHookNext hook)    = ToggleHook.toggleHookNext hook   ? printf "Toggle Hook %s (Next)" hook

instance IsCmd WorkspaceCmd where
  command (WorkspaceOnScreen focus' ps@(PScreen.P s')) = PScreen.getScreen    def ps ?+ (\s -> windows (W.currentTag >>= \x -> onScreen (W.greedyView x) focus' s)) ? printf "View screen %i (%s)" s' (show focus')
  command (WorkspaceSendToScreen   ps@(PScreen.P s')) = PScreen.sendToScreen def ps ? printf "Send workspace to screen %i" s'
  command (WorkspaceViewScreen     ps@(PScreen.P s')) = PScreen.viewScreen   def ps ? printf "View screen %i"              s'
  command (WorkspaceView    ws)  = DynWS.withNthWorkspace W.greedyView ws ? printf "View tag %i" ws
  command (WorkspaceCopy    ws)  = DynWS.withNthWorkspace CW.copy      ws ? printf "Copy focused to tag %i" ws
  command (WorkspaceShiftTo ws)  = DynWS.withNthWorkspace W.shift      ws ? printf "Move focused to tag %i" ws
  -- TODO keys
  command WorkspaceCycleRecentHidden = cycleRecentHiddenWS [xK_Super_L, xK_Alt_L] xK_y xK_p ? "Cycle (focus) recent tags"
  -- TODO DynWS + WSNames
  command WorkspaceAddPrompt     = wsPromptNew' "Add tag: " ?+ DynWS.addWorkspace ? "New tag (XP)"
  command WorkspaceSetNamePrompt = wsPromptNew' "Rename tag: " ?+ (\name -> WSNames.setCurrentWorkspaceName name Control.Monad.>> saveWorkspaces) ? "Rename this tag (XP)"
  command WorkspaceRemoveFocused = removeNoVisibleWS ? "Remove this tag (if empty)"
  command (WorkspaceSwapTo d _)  = WSNames.swapTo d ? printf "Shift current tag %s" (if d == Next then "forward" else "backwards")
  command (FocusScreenIn Next)   = CycleWS.nextScreen ? "Focus next screen"
  command (FocusScreenIn Prev)   = CycleWS.prevScreen ? "Focus previous screen"

  cmdEnum _ = [WorkspaceAddPrompt,WorkspaceCycleRecentHidden,WorkspaceRemoveFocused]

instance IsCmd SetLayoutCmd where
  command ResetLayout         = (asks (X.layoutHook . X.config) >>= setLayout) ? "Reset layout"
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
  command FocusMaster         = BW.focusMaster                                            ? "Focus master (BoringWindows)"
  command FocusUp             = BW.focusUp                                                ? "Focus up (BoringWindows)"
  command FocusDown           = BW.focusDown                                              ? "Focus down (BoringWindows)"
  command FocusUrgent         = focusUrgent                                               ? "Focus urgent window"
  command SwapMaster          = windows W.swapMaster                                      ? "Swap to master"
  command SwapUp              = windows W.swapUp                                          ? "Swap up"
  command SwapDown            = windows W.swapDown                                        ? "Swap down"
  command RotSlavesDown       = RotSlaves.rotSlavesDown                                   ? "Rotate slaves down"
  command RotSlavesUp         = RotSlaves.rotSlavesUp                                     ? "Rotate slaves up"
  command RotAllDown          = RotSlaves.rotAllDown                                      ? "Rotate down"
  command RotAllUp            = RotSlaves.rotAllUp                                        ? "Rotate up"
  command (FocusSwapMaster w) = windows (W.focusWindow w Control.Monad.>> W.swapMaster)   ? "Focus window & swap it master"
  command ToggleFocusedWindowBorder = (withFocused toggleBorder Control.Monad.>> refresh) ? "Toggle focused window border"
  command (MaximizeWindow w)  = XMonad.Actions.Minimize.maximizeWindow w ? "Maximize window"
  command (MinimizeWindow w)  = XMonad.Actions.Minimize.minimizeWindow w ? "Minimize window"
  command MinimizeFocused     = withFocused XMonad.Actions.Minimize.minimizeWindow ? "Minimize Focused"

-- * Misc. hooks

-- similar to DynamicProperty.dynamicPropertyChange, but acting on MapRequestEvents
docksEventHookExtra :: Event -> X All
docksEventHookExtra MapRequestEvent{ev_window = w} = do
    whenX (runQuery ManageDocks.checkDock w) $ do
      SizeHints{sh_win_gravity = wg} <- withDisplay $ \d -> io (getWMNormalHints d w)
      when (wg == Just staticGravity) $ moveWindowPerStrutPartial w
    return (All True)
  where
    (>>) = (Control.Monad.>>)
docksEventHookExtra _ = return (All True)

moveWindowPerStrutPartial :: Window -> X ()
moveWindowPerStrutPartial w = do
    wda  <- getAtom "_NET_WM_DESKTOP"
    wsa  <- getAtom "_NET_WM_STRUT"
    wspa <- getAtom "_NET_WM_STRUT_PARTIAL"
    msp <- WinProp.getProp32 wspa w
    case msp of
      Just sp@[l, r, t, b, ly1, ly2, ry1, ry2, tx1, tx2, bx1, bx2] -> do
        rootw <- asks theRoot
        rwa <- withDisplay $ \d -> io (getWindowAttributes d rootw)
        mda <- WinProp.getProp32 wda w
        case mda of
          Just _ -> return ()
          Nothing -> do
            sr <- withWindowSet $ return . screenRect . W.screenDetail . W.current
            move' rwa sr (calcStruts rwa sr (map fi sp))
      _ -> return ()
  where
    (>>) = (Control.Monad.>>)
    fi = fromIntegral

    calcStruts :: WindowAttributes -> Rectangle -> [Int32] -> [Int32]
    calcStruts rwa sr ps@(l:r:t:b:ly1:ly2:ry1:ry2:tx1:tx2:bx1:bx2:_)
      | l > 0 = [l + fi (rect_x sr),0,0,0, rect_y sr + ly1,rect_y sr + ly2, 0,0, 0,0, 0,0]
      | r > 0 = [0,fi (wa_width rwa) - rect_x sr - fi (rect_width sr) + r,0,0, 0,0, rect_y sr + ry1,rect_y sr + ry2, 0,0, 0,0]
      | t > 0 = [0,0,fi (rect_y sr) + t,0, 0,0, 0,0, rect_x sr + tx1,rect_x sr + max 0 (tx2 - 1), 0,0]
      | b > 0 = [0,0,0,b, 0,0, 0,0, 0,0, rect_x sr + bx1,rect_x sr + max 0 (bx2 - 1)]
      | otherwise = ps
    calcStruts rwa sr ps = ps

    move' :: WindowAttributes -> Rectangle -> [Int32] -> X ()
    move' rwa Rectangle{rect_x = sx, rect_y = sy, rect_width = sw} sp@[l, r, t, b, ly1, ly2, ry1, ry2, tx1, tx2, bx1, bx2] = withDisplay $ \d -> do
      wspa <- getAtom "_NET_WM_STRUT_PARTIAL"
      io (changeProperty32 d w wspa cARDINAL propModeReplace (map fi sp))
      when (l > 0) $ io $ moveWindow d w sx ly1
      when (r > 0) $ io $ moveWindow d w (fi (wa_width rwa) - r) ry1
      when (t > 0) $ io $ moveWindow d w tx1 sy
      when (b > 0) $ io $ moveWindow d w bx1 (fi (wa_height rwa) - b)

    move' rwa _ _ = return ()

myToggleFloatAllNew = do
  FloatNext.toggleFloatAllNew
  next <- FloatNext.willFloatNext
  new <- FloatNext.willFloatAllNew
  Notify.notifyLastS $ if new
    then "Float hook: float all new windows"
    else if next
    then "Float hook: float next window"
    else "Float hook: inactive"
  where
    (>>) = (Control.Monad.>>)
