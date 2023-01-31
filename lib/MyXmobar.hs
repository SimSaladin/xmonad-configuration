{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE ScopedTypeVariables #-}

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
  , xmobarRunMain
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
import qualified Data.List as L
import qualified Data.Set as Set
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
import XMonad.Hooks.NamedLoggers

-- * New stuff

myStatusBars :: XConfig l -> XConfig l
myStatusBars xc = SB.dynamicSBs mkStatusBarConfig xc { logHook = logHook xc <+> namedLoggersLogHook myFocusedPP }

mkStatusBarConfig :: ScreenId -> IO SB.StatusBarConfig
mkStatusBarConfig screenId = do
  hRef <- newIORef Nothing
  return def
    { SB.sbStartupHook = io (readIORef hRef) >>= start hRef
    , SB.sbLogHook     = io (readIORef hRef) >>= (`whenJust` say hRef)
    , SB.sbCleanupHook = io (readIORef hRef) >>= dead hRef
    }
      where
        start hRef Just{}  = trace (printf "error: status bar for screen=%i is still running!" (fromEnum screenId))
        start hRef Nothing = io (myStatusBar screenId) >>= go
          where go r@Just{} = io (writeIORef hRef r)
                go Nothing  = trace (printf "Status bar for screen=%i was NOT started" (fromEnum screenId))

        say hRef h = do
          current <- curScreenId
          let thisPP = if current == screenId then myFocusedPP else myUnfocusedPP
          str <- workspaceNamesPP thisPP >>= dynamicLogString
          io $ E.catch (hPutStrLn h str) (\(_::E.IOException) -> dead hRef (Just h))

        dead hRef mh = io $ do
          whenJust mh $ catchIO . hClose
          writeIORef hRef Nothing
          sbCleanup screenId

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
  <> modifyConfigB (\cfg -> cfg { XB.dpi = 161 }) -- default 96
  <> setFontsB
      [ def { fontFamily = "Noto Sans Mono",         fontSize = Just (PointSize 7) } -- default 0
      , def { fontFamily = "WenQuanYi Micro Hei",    fontSize = Just (PointSize 7) } --, fontOffset = Just 16 } -- CJK 1
      , def { fontFamily = "TerminessTTF Nerd Font", fontSize = Just (PointSize 8) } --, fontOffset = Just 16 } -- symbols 2
      , def { fontFamily = "Noto Sans Symbols2",     fontSize = Just (PointSize 7) } --, fontOffset = Just 18 } -- symbols 3
      , def { fontFamily = "Noto Sans Mono",         fontSize = Just (PointSize 8) } -- monospace 4
      , def { fontFamily = "Noto Sans",              fontSize = Just (PointSize 8) } -- normal 5
      ]
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
    --, alsaB "default" "Master" volumeArgs
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

    symCpu    = fg colBase1 $ xbFontTerminessNerd "\57958 " <> hairsp -- 
    symMem    = fg colBase1 $ xbFontTerminessNerd "\63578 " -- <> hairsp -- 
    symNet    = fg colBase1 $ xbFontTerminessNerd "\62736 " -- <> hairsp -- "?" alt: ⇅
    symBTC    = fg colBase1 $ xbFontTerminessNerd "\63147" <> hairsp -- ""
    symKbd    = fg colBase1 $ xbFontTerminessNerd "\63506 " -- <> hairsp -- ""
    symClock  = fg colBase1 $ xbFontTerminessNerd "\63055 " -- <> hairsp -- ""
    symVolOn  = xbFontTerminessNerd "\61480 " <> hairsp -- ""
    symVolOff = xbFontTerminessNerd "\61478 " <> hairsp -- ""
    symPlay   = xbFontTerminessNerd "\61515 " -- <> hairsp -- "\58882" ">>"
    symPause  = xbFontTerminessNerd "\61516 " -- <> hairsp -- "\63715" "||"
    symStop   = xbFontTerminessNerd "\61517 " -- <> hairsp -- "><"
    -- "🌡" XXX

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
      , monFieldEllipsis = "…"
      , monExtraArgs     = [ "-P", fg colGreen  symPlay , "-Z", fg colYellow symPause , "-S", fg colOrange symStop ]
      } where
          artist = fg colCyan "<artist>"
          title  = fg colBase1 "<title>"
          album  = fg colBase01 $ wrap "「" "」" $ fg colBlue "<album>"
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

-- * Hacky StatusBar processes

-- To keep track of status bars that execute as child processes.
sbarHackRef :: IORef (Map.Map ScreenId [ProcessID])
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
       atomicModifyIORef sbarHackRef $ \xs -> (Map.insertWith (++) screen [pID] xs, ())
       trace (printf "statusbar for screen=%i: logFd=%s pid=%i" (fromEnum sid) (show h) (fromEnum pID))
       return h
    where
      myStatusBar' :: Rectangle -> IO (Handle, ProcessID)
      myStatusBar' sr =
        withNamedLogInputs screen $ \fds -> do
          trace $ printf "Spawning statusbar: screen=%i namedLoggers=%s" (fromEnum screen) (show fds)
          spawnPipeIO $ xmobarRunExec screen sr fds

xmobarRunExec :: ScreenId -> Rectangle -> Map.Map NamedLoggerId Posix.Fd -> IO ()
xmobarRunExec screen sr fds = do
  binDir <- dataDir <$> io getDirectories
  rds <- mapM (fmap printFd . Posix.dup) fds
  exec $ program (binDir ++ "/xmobar-run") [show screen, show sr, show rds]
  where
    printFd :: Posix.Fd -> String
    printFd = printf "/dev/fd/%i" . fromEnum

-- | The main of the xmobar-run executable
xmobarRunMain :: ScreenId -> Rectangle -> Map.Map NamedLoggerId FilePath -> IO ()
xmobarRunMain screen sr rds = myXBConfig screen sr rds >>= XB.xmobar

sbCleanupAll :: MonadIO m => m ()
sbCleanupAll = io (readIORef sbarHackRef) >>= mapM_ (uncurry terminate) . (\xs -> [ (k,p) | (k,ps) <- xs, p <- ps ]) . Map.toList

sbCleanup :: MonadIO m => ScreenId -> m ()
sbCleanup sid = io (readIORef sbarHackRef) >>= mapM_ (terminate sid) . Map.findWithDefault [] sid

terminate :: MonadIO m => ScreenId -> ProcessID -> m ()
terminate sId pId = do
  trace $ printf "Terminating statusbar screen=%i PID=%i" (fromEnum sId) (fromEnum pId)
  catchIO $ do
    void $ Posix.signalProcess Posix.sigTERM pId
    r <- timeout 500000 $ Posix.getProcessStatus True False pId
    case r of
      Nothing -> trace $ printf "Terminating statusbar failed (timeout), PID=%i" (fromEnum pId)
      _       -> trace $ printf "Successfully terminated statusbar process PID=%i" (fromEnum pId)
  io . atomicModifyIORef sbarHackRef $ \xs -> (Map.adjustWithKey (\_ -> L.delete pId) sId xs, ()) -- insertWith (++) screen [pID] xs, h) -- \xs -> ((screen, pID) : xs, h)
  trace $ printf "Cleaned up statusbar screen=%i PID=%i" (fromEnum sId) (fromEnum pId)

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
