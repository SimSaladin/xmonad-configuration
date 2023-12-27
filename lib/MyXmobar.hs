{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE UnicodeSyntax         #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
------------------------------------------------------------------------------
-- |
-- Module      : MyXmobar
-- Copyright   : (c) Samuli Thomasson, 2019
-- License     : BSD-3
--
-- Maintainer  : Samuli Thomasson
-- Stability   : unstable
-- Portability : unportable
--
-- Xmobar configuration.
--
------------------------------------------------------------------------------

module MyXmobar
  ( myStatusBars
  , exitHook
  , xmobarRunMain
  ) where

import           XMonad                               hiding (spawn, title)
import           XMonad.Prelude
import qualified XMonad.StackSet                      as W

import qualified XMonad.Actions.DynamicWorkspaceOrder as DO
import           XMonad.Hooks.Rescreen                (addAfterRescreenHook)
import qualified XMonad.Hooks.StatusBar               as SB
import           XMonad.Hooks.StatusBar.PP            (PP(..), dynamicLogString, pad, shorten, wrap, xmobarRaw)
import qualified XMonad.Util.ExtensibleState          as XS
import           XMonad.Util.Loggers
import           XMonad.Util.PureX

import           Codec.Binary.UTF8.String             (encodeString)
import           Control.Concurrent                   (threadDelay)
import qualified Control.Exception                    as E
import           Control.Monad
import           Data.IORef
import qualified Data.List                            as L
import qualified Data.Map                             as Map
import           Data.Maybe
import qualified Data.Set                             as Set
import           Graphics.X11.Xrandr                  (XRRMonitorInfo(..), xrrGetMonitors)
import           Prelude
import qualified System.IO                            as IO
import           System.IO.Unsafe                     (unsafePerformIO)
import qualified System.Posix                         as Posix
import           System.Timeout                       (timeout)
import           Text.Printf                          (printf)

import           MyRun
import           MyTheme
import           StatusBar.XMobar
import qualified Xmobar                               as XB
import           XMonad.Hooks.NamedLoggers

-- * New stuff

data PerScreenSBParams = PerScreenSBParams
  { sb_screen :: ScreenId
  , sb_rect   :: Rectangle
  , sb_dpi    :: Double
  } deriving (Eq, Show, Read)

-- Extended SB.StatusBarConfig
data PerScreenSB = PerScreenSB
  { sb_h_ref       :: IORef (Maybe Handle)
  , sb_fds         :: Map.Map NamedLoggerId Posix.Fd
  , sb_pid         :: ProcessID
  , sb_params      :: PerScreenSBParams
  , sb_logHook     :: X ()
  , sb_cleanupHook :: X ()
  }

newtype PerScreenSBs = PerScreenSBs { getPerScreenSBs :: Map.Map ScreenId PerScreenSB }

instance ExtensionClass PerScreenSBs where
  initialValue = PerScreenSBs mempty

myStatusBars :: XConfig l -> XConfig l
myStatusBars xc =
  addAfterRescreenHook updatePerScreenSBs $ xc
    { startupHook = startupHook xc >> updatePerScreenSBs
    , logHook     = logHook xc >> namedLoggersLogHook myFocusedPP >> logPerScreenSBs
    }

updatePerScreenSBs :: X ()
updatePerScreenSBs = do
  actualScreens <- withWindowSet $ return . map W.screen . W.screens
  wanted <- catMaybes <$> mapM perScreenSBParams actualScreens
  (toKeep, toKill) <- Map.partition (\sb -> sb_params sb `elem` wanted) . getPerScreenSBs <$> XS.get
  -- kill unnecessary/changed statusbars
  traverse_ sb_cleanupHook toKill
  -- create missing statusbars
  let missing = wanted \\ map sb_params (Map.elems toKeep)
  created <- traverse createSB missing
  XS.put (PerScreenSBs (toKeep <> Map.fromList (map (\sb -> (sb_screen $ sb_params sb, sb)) created)))

logPerScreenSBs :: X ()
logPerScreenSBs = XS.get >>= traverse_ sb_logHook . getPerScreenSBs

perScreenSBParams :: MonadIO m => ScreenId -> m (Maybe PerScreenSBParams)
perScreenSBParams screen = io $ do
  mrect <- getScreenRectangle screen
  dpi <- getScreenDPI screen
  case mrect of
    Nothing   -> return Nothing
    Just rect -> return $ Just $ PerScreenSBParams screen rect dpi

createSB :: PerScreenSBParams -> X PerScreenSB
createSB params@PerScreenSBParams{..} = io $ do
  hRef <- newIORef Nothing
  ((h, pID), fds) <- withNamedLogInputs sb_screen $ \fds -> do
    sb <- spawnPipeIO $ xmobarRunExec sb_screen sb_rect sb_dpi fds
    return (sb, fds)
  writeIORef hRef (Just h)
  -- TODO: unnecessary?
  atomicModifyIORef sbarHackRef $ \xs -> (Map.insertWith (++) sb_screen [pID] xs, ())
  h' <- IO.hShow h
  trace $ printf "mkStatusBarConfig: screen %i: spawned (PID=%i, DPI=%.f, Rectangle=%s, logFd=%s, namedLoggers=%s)"
    (fromEnum sb_screen) (fromEnum pID) sb_dpi (show sb_rect) h' (show fds)
  return PerScreenSB
    { sb_h_ref = hRef
    , sb_fds = fds
    , sb_pid = pID
    , sb_params = params
    , sb_logHook = logSB sb_screen hRef pID
    , sb_cleanupHook = deadSB sb_screen hRef pID
    }

logSB :: ScreenId -> IORef (Maybe Handle) -> ProcessID -> X ()
logSB screen hRef pID = io (readIORef hRef) >>= (`whenJust` log')
  where
    log' h = do
      current <- curScreenId
      let thisPP = if current == screen then myFocusedPP else myUnfocusedPP
      str <- workspaceNamesPP thisPP >>= dynamicLogString
      io $ E.catch (hPutStrLn h str) (\(_::E.IOException) -> deadSB screen hRef pID)

deadSB :: MonadIO m => ScreenId -> IORef (Maybe Handle) -> ProcessID -> m ()
deadSB screen hRef sbPID = io $ do
  mh <- readIORef hRef
  trace (printf "[StatusBar %i] shutting down (handle=%s)" (fromEnum screen) (show mh))
  writeIORef hRef Nothing
  whenJust mh $ catchIO . hClose
  terminate screen sbPID

  -- TODO unnecessary?
  readIORef sbarHackRef >>= mapM_ (terminate screen) . Map.findWithDefault [] screen

sbCleanupAll :: X ()
sbCleanupAll = do
  traverse_ sb_cleanupHook . getPerScreenSBs =<< XS.get
  -- TODO unnecessary?
  io (readIORef sbarHackRef) >>= mapM_ (uncurry terminate) . (\xs -> [ (k,p) | (k,ps) <- xs, p <- ps ]) . Map.toList

exitHook :: X ()
exitHook = cleanupNamedLoggers >> sbCleanupAll

getScreenRectangle :: MonadIO m => ScreenId -> m (Maybe Rectangle)
getScreenRectangle (S sid) = io $ do
  screenInfo <- E.bracket (openDisplay "") closeDisplay getCleanedScreenInfo
  return $ listToMaybe $ drop sid screenInfo

-- | Get screen DPI. Defaults to 96.
getScreenDPI :: ScreenId -> IO Double
getScreenDPI (S sid) = do
  mMonitors <- E.bracket (openDisplay "") closeDisplay $ \dpy -> do
    root <- rootWindow dpy (defaultScreen dpy)
    xrrGetMonitors dpy root True
  case mMonitors of
    Nothing -> trace "getScreenDPI: xrrGetMonitors returned nothing!" >> return 96
    Just monitors ->
      case drop sid monitors of
        xrr_mon : _ -> do
          let width = fi $ xrr_moninf_width xrr_mon :: Double -- 3840
              height = fi $ xrr_moninf_height xrr_mon :: Double -- 2160
              mwidth = fi $ xrr_moninf_mwidth xrr_mon :: Double -- 710
              mheight = fi $ xrr_moninf_mheight xrr_mon :: Double -- 400
              dpi = fi $ round $ (width / mwidth + height / mheight) * 25.4 / 2 :: Double
          trace $ printf "getScreenDPI: screen %i: %.fx%.f %.fmm x %.fmm -> DPI=%.f" (fromEnum sid) width height mwidth mheight dpi
          return dpi
        _ -> trace (printf "getScreenDPI: did not find monitor for screen %i!" (fromEnum sid)) >> return 96

-- * Hacky StatusBar processes

-- To keep track of status bars that execute as child processes.
sbarHackRef :: IORef (Map.Map ScreenId [ProcessID])
sbarHackRef = unsafePerformIO (newIORef mempty)
{-# NOINLINE sbarHackRef #-}

xmobarRunExec :: ScreenId -> Rectangle -> Double -> Map.Map NamedLoggerId Posix.Fd -> IO ()
xmobarRunExec screen sr dpi fds = do
  binDir <- dataDir <$> io getDirectories
  rds <- mapM (fmap printFd . Posix.dup) fds
  exec $ program (binDir ++ "/xmobar-run") [show screen, show sr, show dpi, show rds]
  where
    printFd :: Posix.Fd -> String
    printFd = printf "/dev/fd/%i" . fromEnum

-- | The main of the xmobar-run executable
xmobarRunMain :: ScreenId -> Rectangle -> Double -> Map.Map NamedLoggerId FilePath -> IO ()
xmobarRunMain screen sr dpi rds = myXBConfig screen sr dpi rds >>= XB.xmobar

terminate :: MonadIO m => ScreenId -> ProcessID -> m ()
terminate sId pId = do
  catchIO $ do
    terminateDelay Posix.sigTERM 1000000{-1s-} $
      terminateDelay Posix.sigTERM 2000000{-2s-} $
          terminateDelay Posix.sigKILL 100000{-0.1s-} $
            trace $ printf "[StatusBar %i] ERROR: failed to terminate statusbar process even with KILL! (PID=%i)" (fromEnum sId) (fromEnum pId)

  -- TODO unnecessary?
  io . atomicModifyIORef sbarHackRef $ \xs -> (Map.adjustWithKey (\_ -> L.delete pId) sId xs, ()) -- insertWith (++) screen [pID] xs, h) -- \xs -> ((screen, pID) : xs, h)

  trace $ printf "[StatusBar %i] Cleaned up statusbar (PID=%i)" (fromEnum sId) (fromEnum pId)
    where
      terminateDelay :: Posix.Signal -> Int -> IO () -> IO ()
      terminateDelay signal delay onFail = do
        trace $ printf "[StatusBar %i] Terminating with %i (PID=%i)" (fromEnum sId) (fromEnum signal) (fromEnum pId)
        Posix.signalProcessGroup signal {-Posix.sigTERM-} pId
        threadDelay delay -- 1000000 -- 1s
        r <- E.try @E.IOException $ Posix.getProcessStatus False{-block-} False{-stopped-} pId
        case r of
          Left _         -> return () -- process already exited
          Right (Just r) -> trace $ printf "[StatusBar %i] Terminated with %i (PID=%i) %s" (fromEnum sId) (fromEnum signal) (fromEnum pId) (show r)
          Right Nothing  -> onFail

-- * XMobar Config

-- Fonts
xbFontDefault, xbFontWqyMicroHei, xbFontTerminessNerd, xbFontNotoSymbols2, xbFontMono, xbFontMonoFull :: String -> String
xbFontDefault       = xmobarFont 0 -- CJK
xbFontWqyMicroHei   = xmobarFont 1 -- CJK
xbFontTerminessNerd = xmobarFont 2 -- symbols
xbFontNotoSymbols2  = xmobarFont 3 -- symbols
xbFontMono          = xbFontDefault
xbFontMonoFull      = xmobarFont 4 -- monospace, larger

-- | Generate XMobar config
myXBConfig :: ScreenId -> Rectangle -> Double -> Map.Map NamedLoggerId FilePath -> IO XB.Config
myXBConfig (S sid) sr dpi pipes = fromConfigB $
     modifyConfigB (\cfg -> cfg
       { XB.position = XB.OnScreen sid XB.Top
       , XB.bgColor = colBase03
       , XB.fgColor = colBase0
       , XB.allDesktops = False
       , XB.borderWidth = 0
       , XB.dpi = dpi
       })
  <> setFontsB
      [ def { fontFamily = "NotoSans Nerd Font", {- "Noto Sans Mono", -} fontSize = Just (PointSize 7) } -- default 0
      , def { fontFamily = "WenQuanYi Zen Hei",      fontSize = Just (PointSize 7) } -- CJK 1
      , def { fontFamily = "Terminess Nerd Font Mono" {- "TerminessTTF Nerd Font" -}, fontSize = Just (PointSize 7) } -- symbols 2
      , def { fontFamily = "Noto Sans Symbols2",     fontSize = Just (PointSize 7) } -- symbols 3
      , def { fontFamily = "Noto Sans Mono",         fontSize = Just (PointSize 8) } -- monospace 4
      ]
  <> pipeReaderB "xmonad" "/dev/fd/0"
  <> whenB (widthAtLeast 2500) (litB emspace <> mpdB mpdArgs 50)
  <> litB emspace <> bufferedPipeReaderB [ (time, False, fp) | (k, fp) <- Map.toList pipes, let time = myPipeTimeout k ]
  <> "}"
  <> "{"
  <> batteryB batteryArgs 100
  <> sepByB (litB enspace)
    [ litB symCpu <> multiCpuB multiCpuArgs 50
    , topProcB topProcArgs 50
    , litB symMem <> memoryB memoryArgs 50
    , topMemB topMemArgs 50
    , litB symNet <> dynnetworkB networkArgs 50
    , alsaB "default" "Master" volumeArgs
    , litB symKbd <> kbdAndLocks
    , litB symBTC <> btcPrice 600
    , whenB (widthAtLeast 2500) $ weatherB skyConditions "LOWG" (weatherArgs "Wien") 1800
    , litB symClock <> dateZoneB dateFmt "" "" 10
    ]
  where
    widthAtLeast w = return $ rect_width sr >= w

    myPipeTimeout k = fromMaybe (secs 15) $ lookup k [(NLogTitle, 0)]
    secs n = n * 10

    colLow    = colBase00 -- colBase01
    colNormal = colGreen
    colHigh   = colOrange

    underline = box' def{ boxType = BBottom, boxColor = colBase01, boxMargin = [0,3,0,0] }

    symCpu    = fg colBase1 "\xe266 " -- nf-fae-chip
    symMem    = fg colBase1 "\xf035b " -- nf-md-memory
    symNet    = fg colBase1 "\xf0c9d " -- nf-md-network_outline
    symBTC    = fg colBase1 "\xf15a " -- nf-fa-bitcoin
    symKbd    = fg colBase1 "\xf0313 " -- nf-md-keyboard_variant
    symClock  = fg colBase1 "\xe641 " -- nf-seti-clock
    symVolOn  = "\xf057e " -- nf-md-volume_high
    symVolOff = "\xf0581 " -- nf-md-volume_off
    symPlay   = "\xf040a " -- nf-md-play
    symPause  = "\xf03e4 " -- nf-md-pause
    symStop   = "\xf04db " -- nf-md-stop

    kbdAndLocks = kbdB <> litB hairsp <> fgB colOrange locksB

    btcPrice = comB "cat" ["/tmp/xmobar.ticker"]

    dateFmt = sepByConcat puncsp [weeknum, weekday, daymonth, hourmin <> seconds, zone]
        where
          weeknum  = fg colBase01 "W%V"
          weekday  = fg colBase01 "%a"
          daymonth = fg colBase1 "%-d" <> "." <> fg colBase00 "%-m"
          hourmin  = fg colBase1 "%-H:%M"
          seconds  = xbFontMono (":" <> fg colBase01 "%S")
          zone     = xbFontMono (fg colBase01 "%Z")

    weatherArgs station = def
      { monTemplate  = sepByConcat puncsp [xbFontNotoSymbols2 "<skyConditionS>", station, xbFontMonoFull "<tempC>" <> "℃ ", "<rh>%", "<windKmh>" <> fg colBase01 "km/h"]
      , monHigh      = 20
      , monLow       = 5
      , monHighColor = colOrange
      , monLowColor  = colBlue
      }
    skyConditions =
      [ ("clear", "🌣")
      , ("sunny", "☀")
      , ("mostly clear", "🌤")
      , ("mostly sunny", "🌤")
      , ("partly sunny", "⛅")
      , ("fair", "🌕") -- other: 🌑
      , ("cloudy","☁")
      , ("overcast","☁")
      , ("partly cloudy", "⛅")
      , ("mostly cloudy", "🌧")
      , ("considerable cloudiness", "⛈")
      , ("", "🌡")
      ]

    networkArgs = def
      { monTemplate    = sepByConcat puncsp [dev, tx, rx]
      , monHigh        = 1024 * 1024 -- 1048576
      , monLow         = 128  * 1024 -- 131072
      , monHighColor   = colOrange
      , monNormalColor = colBase1
      , monLowColor    = colBase01
      , monSuffix      = True
      } where
        dev = xbFontWqyMicroHei "<dev>"
        tx  = "<tx>"
        rx  = "<rx>"

    multiCpuArgs = def
      { monTemplate    = xbFontMonoFull "<total>%"
      , monLow         = 25
      , monHigh        = 75
      , monHighColor   = colOrange
      , monNormalColor = colBase1
      , monLowColor    = colBase01
      }

    topProcArgs = def
      { monTemplate    = sepByConcat puncsp $ map fmt1 [1..2]
      , monHigh        = 30 -- for cpu: activity-%, for mem: like in %topmem%
      , monLow         = 10
      , monHighColor   = colOrange
      , monNormalColor = colBase1
      , monLowColor    = colBase01
      } where
        fmt1 :: Int -> String
        fmt1 n = xbFontWqyMicroHei $ wrap hairsp thinsp $ printf "<name%i>" n <> hairsp <> xbFontMono (printf "<cpu%i>" n <> "%")

    topMemArgs = def
      { monTemplate    = sepByConcat puncsp $ map fmt1 [1..2]
      , monHigh        = 25 -- percentages over total memory currently in use
      , monLow         = 8
      , monHighColor   = colOrange
      , monNormalColor = colBase1
      , monLowColor    = colBase01
      } where
        fmt1 :: Int -> String
        fmt1 n = xbFontWqyMicroHei $ wrap hairsp hairsp $ printf "<name%i>" n <> hairsp <> xbFontMono (printf "<mem%i>" n)

    memoryArgs = def
      { monTemplate    = xbFontMonoFull "<usedratio>%"
      , monHigh        = 75
      , monLow         = 25
      , monHighColor   = colOrange
      , monNormalColor = colBase1
      , monLowColor    = colBase01
      }

    volumeArgs = def
      { monTemplate    = "<status>" <> xbFontMonoFull ("<volume>" <> fg colBase01 "%")
      , monLow         = 20
      , monHigh        = 75
      , monHighColor   = colOrange
      , monNormalColor = colBase1
      , monLowColor    = colBase01
      , monExtraArgs   = ["-C", colBase1, "-c", colRed, "-O", symVolOn, "-o", symVolOff]
      }

    mpdArgs = def
      { monTemplate      = sepByConcat thinsp [statei, artist <> fg colBase01 oendash <> title, album, flags]
      , monFieldWidthMax = 30
      , monFieldEllipsis = "…"
      , monExtraArgs     = [ "-P", fg colGreen  symPlay , "-Z", fg colYellow symPause , "-S", fg colOrange symStop ]
      } where
          artist = fg colCyan $ xbFontWqyMicroHei  "<artist>"
          title  = fg colBase1 $ xbFontWqyMicroHei  "<title>"
          album  = fg colBase01 $ wrap "「" "」" $ fg colBlue $ xbFontWqyMicroHei "<album>"
          flags  = fg colBase01 $ wrap "[" "]" $ fg colBase1 $ xbFontMono "<flags>"
          statei = "<statei>"

    batteryArgs = def
      { monTemplate  = "<acstatus><left>% " <> fg colCyan "<timeleft>"
      , monLow       = 15
      , monHigh      = 80
      , monHighColor = colHigh
      , monExtraArgs = ["-O", fg colGreen "AC" <> " ", "-i", "", "-o", ""]
      }

-- * PP

myFocusedPP :: PP
myFocusedPP = def
  { ppUrgent          = fg colGreen
  , ppVisible         = fg colCyan
  , ppCurrent         = fg colMagenta
  , ppHidden          = fg colBase1
  , ppHiddenNoWindows = fg colBase01
  , ppSep             = " "
  , ppLayout          = last . words
  , ppTitle           = xmobarFont 1 . pad . fg colBase1 . xmobarRaw . shorten 128
  --, ppRename          = \s w -> maybe "" (\k -> fg colYellow $ k ++ ":") (Map.lookup (W.tag w) ppTagKeys) ++ s
  , ppOrder           = \(ws : layout : title : xs) -> ws : layout : xs
  }

workspaceNamesPP :: PP -> X PP
workspaceNamesPP pp = do
  wsSort <- DO.getSortByOrder
  tags <- gets (wsSort . W.workspaces . windowset)
  let ppTagKeys = Map.fromList $ zip (map W.tag tags) (map (:[]) ['a'..'z'])
  return pp { ppSort            = DO.getSortByOrder
     --, ppRename = ppRename pp >=>
     , ppRename          = \s w -> maybe "" (\k -> fg colYellow $ k ++ ":") (Map.lookup (W.tag w) ppTagKeys) ++ s
     }

myUnfocusedPP :: PP
myUnfocusedPP = myFocusedPP { ppCurrent = fg colBlue }
