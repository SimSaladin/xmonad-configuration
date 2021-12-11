{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
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

import           XMonad                        hiding (spawn, title)
import qualified XMonad.StackSet               as W

--import           XMonad.Actions.CopyWindow     (wsContainingCopies)
import           XMonad.Actions.WorkspaceNames (workspaceNamesPP)
import qualified XMonad.Hooks.StatusBar        as SB
import           XMonad.Hooks.StatusBar.PP     (dynamicLogString, PP(..), wrap, pad, xmobarRaw, shorten)
import qualified XMonad.Util.ExtensibleState   as XS
import           XMonad.Util.Loggers
import           XMonad.Util.PureX

import           Graphics.X11.Xinerama         (getScreenInfo)

import           Codec.Binary.UTF8.String      (encodeString)
import qualified Control.Exception             as E
import           Control.Monad
import           Data.IORef
import qualified Data.Map                      as Map
import           Data.Maybe
import           Prelude
import qualified System.IO                     as IO
import           System.IO.Unsafe              (unsafePerformIO)
import qualified System.Posix                  as Posix
import           System.Timeout                (timeout)
import           Text.Printf                   (printf)

import           MyRun
import           MyTheme
import           StatusBar.XMobar
import qualified Xmobar                        as XB

-- * New stuff

myStatusBars :: XConfig l -> XConfig l
myStatusBars = SB.dynamicSBs mkStatusBarConfig
  -- TODO: add this log hook only once:
  -- namedLoggersLogHook myFocusedPP

mkStatusBarConfig :: ScreenId -> IO SB.StatusBarConfig
mkStatusBarConfig screenId = do
  res <- myStatusBar screenId
  case res of
    Just h -> return def
      { SB.sbLogHook = do
        when (screenId == S 0) $ namedLoggersLogHook myFocusedPP -- XXX hacky
        current <- curScreenId
        let thisPP = if current == screenId then myFocusedPP else myUnfocusedPP
        str <- run thisPP
        void $ userCode $ io $ hPutStrLn h str
      , SB.sbStartupHook = trace $ "Started status bar for screen: " ++ show screenId
      , SB.sbCleanupHook = void $ io $ partialCleanup screenId
      }
    Nothing -> return def
  where
    run = workspaceNamesPP >=> dynamicLogString

-- * XMobar Config

-- | Generate XMobar config
myXBConfig :: ScreenId -> Rectangle -> Map.Map NamedLoggerId FilePath -> IO XB.Config
myXBConfig (S sid) sr pipes = fromConfigB $
     modifyConfigB (\cfg -> cfg { XB.position = XB.OnScreen sid XB.Top })
  <> modifyConfigB (\cfg -> cfg { XB.bgColor = colBase03, XB.fgColor = colBase0, XB.allDesktops = False })
  <> setFontsB
      -- normal
      [ def { fontFamily = "Noto Sans",              fontSize = Just (PointSize 7) }
      -- cjk
      , def { fontFamily = "WenQuanYi Micro Hei",    fontSize = Just (PointSize 7.5) } -- 1
      -- symbols
      , def { fontFamily = "TerminessTTF Nerd Font", fontSize = Just (PointSize 8.2) } -- 2
      , def { fontFamily = "Noto Sans Symbols2",     fontSize = Just (PointSize 8.5) } -- 3
      -- mono
      , def { fontFamily = "Noto Sans Mono",         fontSize = Just (PointSize 7) } -- 4
      , def { fontFamily = "Noto Sans Mono",         fontSize = Just (PointSize 8) } -- 5
      -- normal
      , def { fontFamily = "Noto Sans",              fontSize = Just (PointSize 8.2) } -- 6
      ]
  <> modifyConfigB (\cfg -> cfg { XB.textOffsets = [-1,19,18,19,-1,-1] })
  --
  <> litB enspace <> wrapB xbFontMonoFull (pipeReaderB "xmonad" "/dev/fd/0")
  <> litB emspace <> mpdB mpdArgs 50
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
    , weatherB skyConditions "LOWG" (weatherArgs "Graz") 1800
    , litB symClock <> dateZoneB dateFmt "" "" 10
    ]
  where
    myPipeTimeout k = fromMaybe (secs 15) $ lookup k [(NLogTitle, 0)]
    secs n = n * 10

    colLow    = colBase00 -- colBase01
    colNormal = colGreen
    colHigh   = colOrange

    underline = box' def{ boxType = BBottom, boxColor = colBase01, boxMargin = [0,3,0,0] }

    xbFontSans          = xmobarFont 0 -- normal
    xbFontSansFull      = xmobarFont 6 -- normal
    xbFontWqyMicroHei   = xmobarFont 1 -- CJK
    xbFontTerminessNerd = xmobarFont 2 -- symbols
    xbFontNotoSymbols2  = xmobarFont 3 -- symbols
    xbFontMono          = xmobarFont 4 -- monospace
    xbFontMonoFull      = xmobarFont 5 -- monospace

    symCpu    = fg colBase1 $ xbFontTerminessNerd "\57958 " <> thinsp -- 
    symMem    = fg colBase1 $ xbFontTerminessNerd "\63578 " -- <> hairsp -- 
    symNet    = fg colBase1 $ xbFontTerminessNerd "\62736 " -- <> hairsp -- "?" alt: ⇅
    symBTC    = fg colBase1 $ xbFontTerminessNerd "\63147" <> hairsp -- ""
    symKbd    = fg colBase1 $ xbFontTerminessNerd "\63506 " -- <> hairsp -- ""
    symClock  = fg colBase1 $ xbFontTerminessNerd "\63055 " -- <> hairsp -- ""
    symVolOn  = xbFontTerminessNerd "\61480 " -- ""
    symVolOff = xbFontTerminessNerd "\61478 " -- ""
    symPlay   = xbFontTerminessNerd "\61515 " -- <> hairsp -- "\58882" ">>"
    symPause  = xbFontTerminessNerd "\61516 " -- <> hairsp -- "\63715" "||"
    symStop   = xbFontTerminessNerd "\61517 " -- <> hairsp -- "><"
    -- "🌡" XXX

    kbdAndLocks =
      wrapB xbFontMono kbdB <> litB hairsp <>
      wrapB xbFontMono (fgB colOrange locksB)

    btcPrice = wrapB xbFontMono . comB "cat" ["/tmp/xmobar.ticker"]

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
      , ("considerable cloudiness", "⛈")]

    networkArgs = def
      { monTemplate    = sepByConcat puncsp [dev, tx, rx]
      , monHigh        = 1024 * 1024 -- 1048576
      , monLow         = 128  * 1024 -- 131072
      , monHighColor   = colOrange
      , monNormalColor = colBase1
      , monLowColor    = colBase01
      , monSuffix      = True
      } where
        dev = "<dev>"
        tx  = xbFontMono "<tx>"
        rx  = xbFontMono "<rx>"

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
        fmt1 n = boxP 2 $ wrap hairsp thinsp $ printf "<name%i>" n <> hairsp <> xbFontMono (printf "<cpu%i>" n <> "%")

    topMemArgs = def
      { monTemplate    = sepByConcat puncsp $ map fmt1 [1..2]
      , monHigh        = 25 -- percentages over total memory currently in use
      , monLow         = 8
      , monHighColor   = colOrange
      , monNormalColor = colBase1
      , monLowColor    = colBase01
      } where
        fmt1 :: Int -> String
        fmt1 n = boxP 2 $ wrap hairsp hairsp $ printf "<name%i>" n <> hairsp <> xbFontMono (printf "<mem%i>" n)

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
      { monTemplate      = xbFontWqyMicroHei $ sepByConcat thinsp [artist <> fg colBase01 oendash <> title, album, statei, xbFontMono flags]
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
  , ppLayout          = last . words
  , ppTitle           = xmobarFont 1 . pad . fg colBase1 . xmobarRaw . shorten 128
  , ppRename          = \s w -> maybe "" (\k -> fg colYellow $ k ++ ":") (Map.lookup (W.tag w) ppTagKeys) ++ s
  , ppOrder           = \(ws : layout : title : xs) -> ws : layout : xs
  } where
    ppTagKeys = Map.fromList $ zip (map show [1..]) (map (:[]) ['a'..'z'])

    -- layoutParts s = xmobarRaw (unwords $ init (words s)) ++ " " ++ fg colMagenta  (xmobarRaw $ last ("":words s))

myUnfocusedPP :: PP
myUnfocusedPP = myFocusedPP { ppCurrent = fg colBlue }

exitHook :: X ()
exitHook = cleanupNamedLoggers >> io cleanup
-- XXX the "cleanup" is likely unnecessary. X.H.StatusBar should take care of that.

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
    logxs <- readIORef namedLoggersRef
    forM_ [h | (mf,mu) <- Map.elems logxs >>= Map.elems, Just h <- [mf,mu]] (catchIO . IO.hClose)

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
          _ -> return ()

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
  let r:rs = drop sid screenInfo
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
        (mapM_ Posix.closeFd)

      printFd = printf "/dev/fd/%i" . fromEnum

cleanup :: IO ()
cleanup = readIORef sbarHackRef >>= mapM_ (destroy . snd)

partialCleanup :: ScreenId -> IO ()
partialCleanup sid = readIORef sbarHackRef >>= mapM_ destroy . lookup sid

destroy :: ProcessID -> IO ()
destroy pId = void $ do
  catchIO $ void $ Posix.signalProcess Posix.sigTERM pId
  catchIO $ void $ Posix.getProcessStatus True False pId

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

 isHidden :: Query Bool
 isHidden = isInProperty "_NET_WM_STATE" "_NET_WM_STATE_HIDDEN"

askScreenRectangle :: ScreenId -> IO Rectangle
askScreenRectangle (S sid) = do
  screenInfo <- E.bracket (openDisplay "") closeDisplay getScreenInfo
  let r:rs = drop sid screenInfo
      rss  = take sid screenInfo
  when (any (r `containedIn`) (rs ++ rss)) $ error "fuck you"
  return r
 -}
