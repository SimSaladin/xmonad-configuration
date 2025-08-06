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
    sb <- spawnPipeIO $ xmobarRunExec params fds
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

-- | Start the subprocess for an xmobar instance.
xmobarRunExec :: PerScreenSBParams -> Map.Map NamedLoggerId Posix.Fd -> IO ()
xmobarRunExec ps fds = do
  exe <- getExe
  rds <- mapM (fmap printFd . Posix.dup) fds
  exec $ program exe [show (sb_screen ps), show (sb_rect ps), show (sb_dpi ps), show rds]
  where
    getExe = io getDirectories >>= \dirs -> pure (dataDir dirs ++ "/xmobar-run")
    printFd :: Posix.Fd -> String
    printFd = printf "/dev/fd/%i" . fromEnum

-- | The main of the xmobar-run executable
xmobarRunMain :: ScreenId -> Rectangle -> Double -> Map.Map NamedLoggerId FilePath -> IO ()
xmobarRunMain sId sr dpi rds = do
  trace $ printf "xmobar(%i): now starting %ix%i+%i+%i [DPI: %.1f]" (fromEnum sId) (rect_width sr) (rect_height sr) (rect_x sr) (rect_y sr) dpi
  c <- myXBConfig sId sr dpi rds
  -- trace $ printf "xmobar(%i): %s" (fromEnum sId) (show c) -- XXX the output gets garbled somehow (buffering?)
  XB.xmobar c

terminate :: MonadIO m => ScreenId -> ProcessID -> m ()
terminate sId pId = do
  catchIO $ do
    terminateDelay Posix.sigTERM 1000000{-1s-} cleanup $
      terminateDelay Posix.sigTERM 2000000{-2s-} cleanup $
          terminateDelay Posix.sigKILL 100000{-0.1s-} cleanup $
            trace $ printf "[StatusBar %i]: ERROR: Failed to terminate statusbar process even with KILL! (PID=%i)" (fromEnum sId) (fromEnum pId)
    where
      terminateDelay :: Posix.Signal -> Int -> IO () -> IO () -> IO ()
      terminateDelay signal delay onSuccess onFail = do
        trace $ printf "[StatusBar %i]: Attempting to terminate by signal: %i. (PID=%i)" (fromEnum sId) (fromEnum signal) (fromEnum pId)
        Posix.signalProcessGroup signal pId
        threadDelay delay
        r <- E.try @E.IOException $ Posix.getProcessStatus False{-block-} False{-stopped-} pId
        case r of
          Right Nothing  -> onFail -- still alive
          Right (Just r) -> do trace $ printf "[StatusBar %i]: Child process terminated: %s" (fromEnum sId) (show r)
                               onSuccess
          Left err       -> do trace $ printf "[StatusBar %i]: Failed to retrieve child process status (PID=%i): %s" (fromEnum sId) (fromEnum pId) (show err)
                               onSuccess

      -- TODO unnecessary?
      cleanup = io . atomicModifyIORef sbarHackRef $ \xs -> (Map.adjustWithKey (\_ -> L.delete pId) sId xs, ())

-- * XMobar Config

-- | Default font size (pointsize)
myFontSize :: Double
myFontSize = 7

-- | XMobar fonts (Pango format).
fonts :: [(String, String)]
fonts =
 [ ("default", printf "NotoSans Nerd Font %.2f" myFontSize)
 , ("mono"   , printf "NotoMono Nerd Font %.2f" (myFontSize * 0.8))
 , ("tiny"   , printf "WenQuanYi Micro Hei %.2f" (myFontSize * 0.9))
 , ("cjk"    , printf "WenQuanYi Zen Hei %.2f" myFontSize)
 , ("noto-color-emoji", printf "Noto Color Emoji %.2f" myFontSize)
 ]

-- | XMobar string with font by key.
fn :: String -> String -> String
fn x = xmobarFont (fromMaybe 0 (findIndex ((x ==) . fst) fonts))

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
       , XB.font = snd mainFont
       , XB.additionalFonts = map snd altFonts
       , XB.textOffset = 0 -- negative to center vertically..?
       })
  <> pipeReaderB "xmonad" "/dev/fd/0"
  <> whenB (widthAtLeast 2500) (litB emspace <> mpdB mpdArgs 50)
  <> litB emspace <> bufferedPipeReaderB [ (time, False, fp) | (k, fp) <- Map.toList pipes, let time = myPipeTimeout k ]
  <> "}"
  <> "{"
  -- <> litB (fn "noto-color-emoji" $ concatMap snd skyConditions)
  <> batteryB batteryArgs 100
  <> sepByB (litB enspace)
    [ litB symCpu <> multiCpuB multiCpuArgs 50
    , topProcB (topProcArgs $ ifWiderThan 1920 2 1) 50
    , litB symMem <> memoryB memoryArgs 50
    , topMemB (topMemArgs $ ifWiderThan 1920 2 1) 50
    , litB symNet <> dynnetworkB networkArgs 50
    , alsaB "default" "Master" volumeArgs
    , litB symKbd <> kbdAndLocks
    , litB symBTC <> btcPrice 600
    , whenB (widthAtLeast 2500) $ weatherB skyConditions "LOWG" (weatherArgs "Wien") 1800
    , litB symClock <> dateZoneB dateFmt "" "" 10
    ]
  where
    mainFont : altFonts = fonts
    ifWiderThan w wide narrow = if fi (rect_width sr) / (dpi / 96) > w then wide else narrow
    widthAtLeast w = return $ rect_width sr >= w

    myPipeTimeout k = fromMaybe (secs 15) $ lookup k [(NLogTitle, 0)]

    secs n = n * 10

    underline = box' def{ boxType = BBottom, boxColor = colBase01, boxMargin = [0,3,0,0] }

    symCpu    = "\xf4bc " -- nf-oct-cpu
             -- "\xe266 " -- nf-fae-chip
    -- symMem    = {- fg colBase1 -} "\xf035b " -- nf-md-memory
    symMem    = {- fg colBase1 -} "\xe266 " -- nf-fae-chip
    symNet    = {- fg colBase1 -} "\xf0c9d " -- nf-md-network_outline
    symBTC    = {- fg colBase1 -} "\xf15a " -- nf-fa-bitcoin
    symKbd    = {- fg colBase1 -} "\xf0313 " -- nf-md-keyboard_variant
    symClock  = {- fg colBase1 -} "\xe641 " -- nf-seti-clock
    symVolOn  = "\xf057e " -- nf-md-volume_high
    symVolOff = "\xf0581 " -- nf-md-volume_off
    symPlay   = "\xf040a" -- nf-md-play
    symPause  = "\xf03e4" -- nf-md-pause
    symStop   = "\xf04db" -- nf-md-stop

    kbdAndLocks = kbdB <> litB hairsp <> fgB colOrange locksB

    btcPrice = comB "/usr/bin/env" ["OUT=/dev/stdout", "kraken_dzen"]

    dateFmt = sepByConcat puncsp [weeknum, weekday, daymonth, hourmin <> seconds, zone]
        where
          weeknum  = fg colBase01 "W%V"
          weekday  = fg colBase01 "%a"
          daymonth = fg colBase1 "%-d" <> "." <> fg colBase00 "%-m"
          hourmin  = fg colBase1 "%-H:%M"
          seconds  = fn "mono" (":" <> fg colBase01 "%S")
          zone     = fn "mono" (fg colBase01 "%Z")

    defArgs = def
      { monLowColor    = colBlue
      , monNormalColor = colYellow
      , monHighColor   = colOrange
      }

    weatherArgs station = defArgs
      { monTemplate  = sepByConcat puncsp
          [ fn "noto-color-emoji" "<skyConditionS>"
          , station
          , fn "mono" "<tempC>" <> "℃ "
          , "<rh>%"
          , "<windKmh>" <> fg colBase01 (fn "tiny" "km/h")
          ]
      , monHigh      = 20
      , monLow       = 5
      }

    skyConditions =
      [ ("clear"                   , "\x2600") -- 0/8
      , ("sunny"                   , "\x2600") -- 0/8
      , ("mostly clear"            , "\x1F324") -- 1/8-2/8 cloud coverage
      , ("mostly sunny"            , "\x1F324") -- 1/8-2/8 cloud coverage
      , ("partly cloudy"           , "\x26C5")  -- 3/8-4/8
      , ("partly sunny"            , "\x26C5")  -- 3/8-4/8
      , ("mostly cloudy"           , "\x1F325")  -- 5/8-7/8
      , ("considerable cloudiness" , "\x1F325")   -- 5/8-7/8
      , ("cloudy"                  , "\x2601")  -- 8/8
      , ("fair"                    , "🌕")      -- Less than 4/10 opaque clouds, no precipitation, no extremes of visibility/temperature/wind
      , ("overcast"                , "\x1F327") -- rainy weather
      , (""                        , "??")
      -- "\x26C8" -- thunderstorm
      ]

    networkArgs = defArgs
      { monTemplate    = fn "tiny" $ sepByConcat puncsp ["<dev>", "<tx>", "<rx>"]
      , monHigh        = 1024 * 1024 -- 1048576
      , monLow         = 128  * 1024 -- 131072
      , monLowColor    = colBase01
      , monSuffix      = True
      }

    multiCpuArgs = defArgs
      { monTemplate    = fn "mono" "<total>%"
      , monLow         = 25
      , monHigh        = 75
      }

    topProcArgs numN = defArgs
      { monTemplate    = fn "tiny" $ sepByConcat puncsp $ map fmt1 [1..numN]
      , monHigh        = 30 -- for cpu: activity-%, for mem: like in %topmem%
      , monLow         = 10
      } where
        fmt1 :: Int -> String
        fmt1 n = printf "<name%i>" n <> hairsp <> fn "mono" (printf "<cpu%i>%%" n)

    topMemArgs numN = defArgs
      { monTemplate    = fn "tiny" $ sepByConcat puncsp $ map fmt1 [1..numN]
      , monHigh        = 35 -- percentages over total memory currently in use
      , monLow         = 15
      , monNormalColor = colBlue
      , monHighColor   = colYellow
      } where
        fmt1 :: Int -> String
        fmt1 n = printf "<name%i>" n <> hairsp <> fn "mono" (printf "<mem%i>" n)

    memoryArgs = defArgs
      { monTemplate    = fn "mono" "<usedratio>%"
      , monHigh        = 75
      , monLow         = 25
      --, monNormalColor = colBase1
      --, monLowColor    = colBase01
      }

    volumeArgs = defArgs
      { monTemplate    = "<status>" <> fn "mono" ("<volume>" <> fg colBase01 "%")
      , monLow         = 20
      , monHigh        = 75
      , monNormalColor = colCyan
      , monExtraArgs   = ["-C", colBase1, "-c", colRed, "-O", symVolOn, "-o", symVolOff]
      }

    mpdArgs = defArgs
      { monTemplate = sepByConcat thinsp
          [ "<statei>"
          , flags
          , artist <> fg colBase01 oendash <> title
          , album
          ]
      , monFieldWidthMax = 30
      , monFieldEllipsis = "…"
      , monExtraArgs     =
        [ "-P", fg colGreen  symPlay
        , "-Z", fg colYellow symPause
        , "-S", fg colOrange symStop
        ]
      } where
          artist = fn "tiny" $ fg colCyan "<artist>"
          title  = fn "tiny" $ fg colBase1 "<title>"
          album  = fg colBase01 $ wrap "「" "」" $ fg colBlue $ fn "tiny" "<album>"
          flags  = fn "mono" $ fg colBase01 $ wrap "[" "]" $ fg colBase1 "<flags>"

    batteryArgs = defArgs
      { monTemplate  = "<acstatus><left>% " <> fg colCyan "<timeleft>"
      , monLow       = 15
      , monHigh      = 80
      , monExtraArgs =
        [ "-O", fg colGreen "AC" <> " "
        , "-i", ""
        , "-o", ""
        ]
      }

-- * XMonad reporting (PP)

myFocusedPP :: PP
myFocusedPP = def
  { ppUrgent          = fg colGreen
  , ppVisible         = fg colCyan
  , ppCurrent         = fg colMagenta
  , ppHidden          = fg colBase1
  , ppHiddenNoWindows = fg colBase01
  , ppSep             = " "
  , ppLayout          = last . words
  , ppTitle           = fn "tiny" . pad . fg colBase1 . xmobarRaw . shorten 32
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
