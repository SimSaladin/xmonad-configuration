{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
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
  ) where

import           XMonad                               hiding (spawn, title)
import qualified XMonad.StackSet                      as W

--import           XMonad.Actions.CopyWindow     (wsContainingCopies)
--import           XMonad.Actions.WorkspaceNames (workspaceNamesPP)
import qualified XMonad.Actions.DynamicWorkspaceOrder as DO
import qualified XMonad.Hooks.StatusBar               as SB
import           XMonad.Hooks.StatusBar.PP            (PP(..), dynamicLogString, pad, shorten, wrap, xmobarRaw)
import qualified XMonad.Util.ExtensibleState          as XS
import           XMonad.Util.Loggers
import           XMonad.Util.PureX

import           Graphics.X11.Xinerama                (getScreenInfo)

import           Codec.Binary.UTF8.String             (encodeString)
import qualified Control.Exception                    as E
import           Control.Monad
import           Data.IORef
import qualified Data.Map                             as Map
import           Data.Maybe
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

-- * New stuff

myStatusBars :: XConfig l -> XConfig l
myStatusBars xc = SB.dynamicSBs mkStatusBarConfig xc { logHook = logHook xc <+> namedLoggersLogHook myFocusedPP }

mkStatusBarConfig :: ScreenId -> IO SB.StatusBarConfig
mkStatusBarConfig screenId = do
  res <- myStatusBar screenId
  case res of
    Just h -> return def
      { SB.sbLogHook = run h
      , SB.sbStartupHook = trace $ "Started status bar for screen: " ++ show screenId
      , SB.sbCleanupHook = sbCleanup screenId
      }
    Nothing -> return def
  where
    run h = do
      current <- curScreenId
      let thisPP = if current == screenId then myFocusedPP else myUnfocusedPP
      str <- workspaceNamesPP thisPP >>= dynamicLogString
      void $ userCode $ io $ hPutStrLn h str

-- * XMobar Config

-- Fonts
xbFontDefault       ,xbFontWqyMicroHei   ,xbFontTerminessNerd ,xbFontNotoSymbols2  ,xbFontMono          ,xbFontMonoFull      ,xbFontSansFull      :: String -> String
xbFontDefault       = xmobarFont 0 -- CJK
xbFontWqyMicroHei   = xmobarFont 1 -- CJK
xbFontTerminessNerd = xmobarFont 2 -- symbols
xbFontNotoSymbols2  = xmobarFont 3 -- symbols
xbFontMono          = xbFontDefault
xbFontMonoFull      = xmobarFont 4 -- monospace, larger
xbFontSansFull      = xmobarFont 5 -- sans-serif, larger

-- | Generate XMobar config
myXBConfig :: ScreenId -> Rectangle -> Map.Map NamedLoggerId FilePath -> IO XB.Config
myXBConfig (S sid) sr pipes = fromConfigB $
     modifyConfigB (\cfg -> cfg { XB.position = XB.OnScreen sid XB.Top })
  <> modifyConfigB (\cfg -> cfg { XB.bgColor = colBase03, XB.fgColor = colBase0, XB.allDesktops = False })
  <> modifyConfigB (\cfg -> cfg { XB.borderWidth = 0 })
  <> ifB (widthAtLeast 2500)
    (setFontsB
      [ def { fontFamily = "Noto Sans Mono",         fontSize = Just (PointSize 7) } -- default 0
      , def { fontFamily = "WenQuanYi Micro Hei",    fontSize = Just (PointSize 7), fontOffset = Just 16 } -- CJK 1
      , def { fontFamily = "TerminessTTF Nerd Font", fontSize = Just (PointSize 8), fontOffset = Just 16 } -- symbols 2
      , def { fontFamily = "Noto Sans Symbols2",     fontSize = Just (PointSize 7), fontOffset = Just 18 } -- symbols 3
      , def { fontFamily = "Noto Sans Mono",         fontSize = Just (PointSize 8) } -- monospace 4
      , def { fontFamily = "Noto Sans",              fontSize = Just (PointSize 8) } -- normal 5
      ]
    )
    (setFontsB
      [ def { fontFamily = "Noto Sans Mono",         fontSize = Just (PointSize 6) } -- default 0
      , def { fontFamily = "WenQuanYi Micro Hei",    fontSize = Just (PointSize 6), fontOffset = Just 13 } -- CJK 1
      , def { fontFamily = "TerminessTTF Nerd Font", fontSize = Just (PointSize 8), fontOffset = Just 16 } -- symbols 2
      , def { fontFamily = "Noto Sans Symbols2",     fontSize = Just (PointSize 7), fontOffset = Just 18 } -- symbols 3
      , def { fontFamily = "Noto Sans Mono",         fontSize = Just (PointSize 6) } -- monospace 4
      , def { fontFamily = "Noto Sans",              fontSize = Just (PointSize 6) } -- normal 5
      ]
    )
  <> litB enspace <> pipeReaderB "xmonad" "/dev/fd/0"
  <> whenB (widthAtLeast 2500) (litB emspace <> mpdB mpdArgs 50)
  <> "}"
  <> litB emspace <> bufferedPipeReaderB [ (time, False, fp) | (k, fp) <- Map.toList pipes, let time = myPipeTimeout k ]
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
    , whenB (widthAtLeast 2500) $ weatherB skyConditions "LOWG" (weatherArgs "Graz") 1800
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

    symCpu    = fg colBase1 $ xbFontTerminessNerd "\57958 " <> hairsp -- ???
    symMem    = fg colBase1 $ xbFontTerminessNerd "\63578 " -- <> hairsp -- ???
    symNet    = fg colBase1 $ xbFontTerminessNerd "\62736 " -- <> hairsp -- "?" alt: ???
    symBTC    = fg colBase1 $ xbFontTerminessNerd "\63147" <> hairsp -- "???"
    symKbd    = fg colBase1 $ xbFontTerminessNerd "\63506 " -- <> hairsp -- "???"
    symClock  = fg colBase1 $ xbFontTerminessNerd "\63055 " -- <> hairsp -- "???"
    symVolOn  = xbFontTerminessNerd "\61480 " <> hairsp -- "???"
    symVolOff = xbFontTerminessNerd "\61478 " <> hairsp -- "???"
    symPlay   = xbFontTerminessNerd "\61515 " -- <> hairsp -- "\58882" ">>"
    symPause  = xbFontTerminessNerd "\61516 " -- <> hairsp -- "\63715" "||"
    symStop   = xbFontTerminessNerd "\61517 " -- <> hairsp -- "><"
    -- "????" XXX

    kbdAndLocks = kbdB <> litB hairsp <> fgB colOrange locksB

    btcPrice = comB "cat" ["/tmp/xmobar.ticker"]

    dateFmt = sepByConcat puncsp [weeknum, weekday, daymonth, hourmin <> seconds, zone]
        where
          weeknum  = fg colBase01 "W%V"
          weekday  = fg colBase01 "%a"
          daymonth = fg colBase1 "%-d" <> "." <> fg colBase00 "%-m"
          hourmin  = xbFontSansFull (fg colBase1 "%-H:%M")
          seconds  = xbFontMono (":" <> fg colBase01 "%S")
          zone     = xbFontMono (fg colBase01 "%Z")

    weatherArgs station = def
      { monTemplate  = sepByConcat puncsp [xbFontNotoSymbols2 "<skyConditionS>", station, xbFontMonoFull "<tempC>" <> "??? ", "<rh>%", "<windKmh>" <> fg colBase01 "km/h"]
      , monHigh      = 20
      , monLow       = 5
      , monHighColor = colOrange
      , monLowColor  = colBlue
      }
    skyConditions =
      [ ("clear", "????")
      , ("sunny", "???")
      , ("mostly clear", "????")
      , ("mostly sunny", "????")
      , ("partly sunny", "???")
      , ("fair", "????") -- other: ????
      , ("cloudy","???")
      , ("overcast","???")
      , ("partly cloudy", "???")
      , ("mostly cloudy", "????")
      , ("considerable cloudiness", "???")
      , ("", "????")
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
        fmt1 n = boxP 2 $ xbFontWqyMicroHei $ wrap hairsp thinsp $ printf "<name%i>" n <> hairsp <> xbFontMono (printf "<cpu%i>" n <> "%")

    topMemArgs = def
      { monTemplate    = sepByConcat puncsp $ map fmt1 [1..2]
      , monHigh        = 25 -- percentages over total memory currently in use
      , monLow         = 8
      , monHighColor   = colOrange
      , monNormalColor = colBase1
      , monLowColor    = colBase01
      } where
        fmt1 :: Int -> String
        fmt1 n = boxP 2 $ xbFontWqyMicroHei $ wrap hairsp hairsp $ printf "<name%i>" n <> hairsp <> xbFontMono (printf "<mem%i>" n)

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
      { monTemplate      = xbFontWqyMicroHei $ sepByConcat thinsp [xbFontDefault statei, artist <> fg colBase01 oendash <> title, album, xbFontMono flags]
      , monFieldWidthMax = 30
      , monFieldEllipsis = "???"
      , monExtraArgs     = [ "-P", fg colGreen  symPlay , "-Z", fg colYellow symPause , "-S", fg colOrange symStop ]
      } where
          artist = fg colCyan "<artist>"
          title  = fg colBase1 "<title>"
          album  = fg colBase01 $ wrap "???" "???" $ fg colBlue "<album>"
          flags  = fg colBase01 $ wrap "[" "]" $ fg colBase1 "<flags>"
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

exitHook :: X ()
exitHook = cleanupNamedLoggers >> io sbCleanupAll
-- XXX the "sbCleanupAll" is likely unnecessary. X.H.StatusBar should take care of that.

-- * Named Loggers

data NamedLoggerId = NLogTitle | NLogLayout
  deriving (Show, Eq, Ord, Enum, Bounded)

newtype NamedLoggersXS = NamedLoggersXS { nlLasts :: Map.Map (NamedLoggerId, ScreenId) String }

instance ExtensionClass NamedLoggersXS where
  initialValue = NamedLoggersXS mempty

type NamedLoggers = Map.Map NamedLoggerId (Map.Map ScreenId (Maybe Handle, Maybe Handle)) -- (focused, unfocused)

namedLoggersRef :: IORef NamedLoggers
namedLoggersRef = unsafePerformIO (newIORef mempty)
{-# NOINLINE namedLoggersRef #-}

mkLogFd :: ScreenId -> NamedLoggerId -> IO (NamedLoggerId, Posix.Fd)
mkLogFd s k = do
  (rd,wd) <- Posix.createPipe
  Posix.setFdOption wd Posix.CloseOnExec True
  wh <- Posix.fdToHandle wd
  IO.hSetBuffering wh IO.LineBuffering
  addLogListener k s (Just wh, Nothing)
  pure (k,rd)

addLogListener :: NamedLoggerId -> ScreenId -> (Maybe Handle, Maybe Handle) -> IO () -- X ()
addLogListener k s hs = io $ atomicModifyIORef namedLoggersRef $ \m -> (Map.insertWith (<>) k (Map.singleton s hs) m, ())

cleanupNamedLoggers :: MonadIO m => m ()
cleanupNamedLoggers = io $ do
    trace "Cleaning up named loggers..."
    logxs <- readIORef namedLoggersRef
    forM_ [h | (mf,mu) <- Map.elems logxs >>= Map.elems, Just h <- [mf,mu]] (catchIO . IO.hClose)
    trace "Named loggers cleaned up."

namedLogLazy :: NamedLoggerId -> Logger -> X ()
namedLogLazy k lgr = lgr >>= \ms -> whenJust ms logIt
  where
    logIt str = do
      sid <- curScreenId
      res <- XS.gets (Map.lookup (k, sid) . nlLasts)
      when (res /= Just str) (namedLogString k str)

namedLogString :: NamedLoggerId -> String -> X ()
namedLogString k str = do
    s <- curScreenId
    logxs <- io (readIORef namedLoggersRef)
    whenJust (logxs Map.!? k) $ \slogm -> do
      XS.modify $ \xs -> xs { nlLasts = Map.alter (const $ Just str) (k,s) (nlLasts xs) }
      void $ Map.traverseWithKey (writeBoth s) slogm
  where
    writeBoth s s' (mf,mu) = whenJust (if s == s' then mf else mu) $ \h ->
      catchIO $ do
        r <- timeout 100000 $ hPutStrLn h $ encodeString str
        case r of
          Nothing -> trace "namedLogString: timeout reached!!"
          _       -> return ()

namedLoggersLogHook :: PP -> X ()
namedLoggersLogHook pp = do
  namedLogLazy NLogTitle  (ppTitle pp  `onLogger` logTitle)
  namedLogLazy NLogLayout logLayout

-- * Hacky StatusBar processes

-- To keep track of status bars that execute as child processes.
sbarHackRef :: IORef [(ScreenId, ProcessID)]
sbarHackRef = unsafePerformIO (newIORef mempty)
{-# NOINLINE sbarHackRef #-}

myStatusBar :: ScreenId -> IO (Maybe Handle)
myStatusBar screen@(S sid) = do
  screenInfo <- E.bracket (openDisplay "") closeDisplay getScreenInfo
  let r:rs = drop sid screenInfo -- TODO non-exhaustive pattern
      rss  = take sid screenInfo
  if any (r `containedIn`) (rs ++ rss)
     then return Nothing
     else fmap Just $ do
       sb@(h,pID) <- myStatusBar' r
       atomicModifyIORef sbarHackRef $ \xs -> ((screen, pID) : xs, h)
    where
      myStatusBar' :: Rectangle -> IO (Handle, ProcessID)
      myStatusBar' sr =
        withNamedLogInputs $ \fds ->
          spawnPipeIO $ mapM (fmap printFd . Posix.dup) fds >>= myXBConfig screen sr >>= XB.xmobar

      withNamedLogInputs :: (Map.Map NamedLoggerId Posix.Fd -> IO a) -> IO a
      withNamedLogInputs = E.bracket
        (Map.fromList <$> mapM (mkLogFd screen) [minBound..maxBound])
        closeLogFds

      closeLogFds :: Map.Map NamedLoggerId Posix.Fd -> IO ()
      closeLogFds = mapM_ $ catchIO . Posix.closeFd

      printFd = printf "/dev/fd/%i" . fromEnum

sbCleanupAll :: MonadIO m => m ()
sbCleanupAll = io (readIORef sbarHackRef) >>= mapM_ (terminate . snd)

sbCleanup :: MonadIO m => ScreenId -> m ()
sbCleanup sid = io (readIORef sbarHackRef) >>= mapM_ terminate . lookup sid

terminate :: MonadIO m => ProcessID -> m ()
terminate pId = do
  trace $ printf "Terminating statusbar process PID=%i" (fromEnum pId)
  catchIO $ do
    void $ Posix.signalProcess Posix.sigTERM pId
    r <- timeout 500000 $ Posix.getProcessStatus True False pId
    case r of
      Nothing -> trace $ printf "Terminating statusbar failed (timeout), PID=%i" (fromEnum pId)
      _       -> trace $ printf "Successfully terminated statusbar process PID=%i" (fromEnum pId)
  trace $ printf "Cleaned up statusbar process PID=%i" (fromEnum pId)

-- * TODO

{-
 - TODO since X.H.StatusBar{,.PP} refactoring:
 - ...
 - copies  <- wsContainingCopies
 - hiddens <- filterM (runQuery isHidden) (W.allWindows wset)
 - ...
 -   | W.tag w `elem` copies                                = ppCopies
 - ...
 -   | Just _ <- W.stack w >>= W.filter (`notElem` hiddens) = ppHidden pp
 -
ppCopies = fg colYellow
-}
