{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE DeriveDataTypeable        #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE PartialTypeSignatures     #-}
{-# LANGUAGE RebindableSyntax          #-}
{-# LANGUAGE RecordWildCards           #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}

------------------------------------------------------------------------------
-- |
-- Module      : MyDebug
-- Description : Misc. debug utilities
-- Copyright   : (c) Samuli Thomasson, 2020
-- License     : WTFPL
--
-- Maintainer  : Samuli Thomasson <samuli.thomasson@paivola.fi>
-- Stability   : unstable
-- Portability : unportable
--
-- Longer description of this module.
--
------------------------------------------------------------------------------

module MyDebug (
  debugIgnoreProps, debugEventHook, debugEventsFocusedWindow, myDebugManageHook, DebugCmd(..)
  ) where

import           GHC.Word                         (Word64)
import           XMonad
import qualified XMonad.Hooks.DebugEvents         as DebugEvents
import qualified XMonad.Hooks.DebugStack          as DebugStack
import qualified XMonad.Hooks.ManageDebug
import           XMonad.Prelude
import qualified XMonad.Util.DesktopNotifications as Notify
import qualified XMonad.Util.ExtensibleState      as XS
import qualified XMonad.Util.Invisible            as Invisible
import           XMonad.Util.NamedCommands


import qualified Data.Set                         as Set
import           Prelude

data DebugXS = DebugXS { debugIgnoreProps   :: !(Atom -> Bool)
                       , debugEnableWindows :: !(Invisible.Invisible Maybe (Set.Set Window))
                       , debugEnableNext    :: !Bool
                       }

instance ExtensionClass DebugXS where
  initialValue = DebugXS (const True) (pure mempty) False

data DebugCmd = DebugManageNext
              | DebugStackSet
              | DebugEnableFocused
              | DebugEnableNext
              deriving (Show, Data)

instance IsCmd DebugCmd where
  command DebugManageNext    = XMonad.Hooks.ManageDebug.debugNextManagedWindow ? "Debug next managed window (ManageDebug)"
  command DebugStackSet      = (DebugStack.debugStackString >>= \str -> trace ("print debug: \n" <> str) >> Notify.notifyLastS str) ? "Debug stack set (DebugStack)"
  command DebugEnableFocused = withFocused debugEventsWindow ? "Enable event debugging for focused window"
  command DebugEnableNext    = debugEventsNextWindow ? "Enable event debugging for next new window"

debugEventHook :: Event -> X All
debugEventHook e = do
  ignoreProp <- XS.gets debugIgnoreProps
  wEnabled <- XS.gets (flip Set.member . Invisible.fromIMaybe mempty . debugEnableWindows)
  case e of
    ConfigureRequestEvent{..} | wEnabled ev_window       -> trace (show e) >> return (All True)
    ConfigureEvent{..}        | wEnabled ev_window       -> DebugEvents.debugEventsHook e
    MapRequestEvent{}                                    -> DebugEvents.debugEventsHook e
    MapNotifyEvent{..}        | wEnabled ev_window       -> DebugEvents.debugEventsHook e
    UnmapEvent{..}            | wEnabled ev_window       -> DebugEvents.debugEventsHook e
    DestroyWindowEvent{..}    | wEnabled ev_window       -> DebugEvents.debugEventsHook e
    PropertyEvent{..}         | wEnabled ev_window       -> DebugEvents.debugEventsHook e
    PropertyEvent{..}         | not (ignoreProp ev_atom) -> DebugEvents.debugEventsHook e
    ExposeEvent{..}           | wEnabled ev_window       -> DebugEvents.debugEventsHook e
    ClientMessageEvent{..}    | wEnabled ev_window       -> DebugEvents.debugEventsHook e
    ClientMessageEvent{ev_message_type = a, ev_data = vs} | (s:v1:v2:vals) <- vs    -> do
      a' <- getAtom "_NET_WM_STATE"
      a_cs <- getAtom "WM_CHANGE_STATE"
      when (a == a_cs) $ void $ DebugEvents.debugEventsHook e
      when (a == a') $ do
        x1 <- dumpAtom v1
        x2 <- dumpAtom v2
        trace $ "CLIENT MESSAGE: type=_NET_WM_STATE, data(0)=" ++ show s ++ ", data(1)=" ++ show x1 ++ ", data(2)=" ++ show x2 ++ ", data(3-N)=" ++ show vals
      return (All True)
    RRScreenChangeNotifyEvent{..} -> trace (show e) >> return (All True)
    _                                                    -> return (All True)

dumpAtom :: Integral a => a -> X String
dumpAtom i = atomName $ fromIntegral i

atomName :: Word64 -> X String
atomName 0 = pure "0"
atomName a =  withDisplay $ \d ->
  io $ fromMaybe ("(unknown atom " ++ show a ++ ")") <$> getAtomName d a

debugEventsWindow :: Window -> X ()
debugEventsWindow w = do
  trace $ "Debugging window: " ++ show w
  XS.modify (\xs -> xs { debugEnableWindows = pure $ Set.insert w (Invisible.fromIMaybe mempty $ debugEnableWindows xs) })

debugEventsFocusedWindow :: X ()
debugEventsFocusedWindow = withFocused debugEventsWindow

debugEventsNextWindow :: X ()
debugEventsNextWindow = XS.modify (\xs -> xs { debugEnableNext = True })

myDebugManageHook :: ManageHook
myDebugManageHook = do
  go <- liftX $ do
    go <- XS.gets debugEnableNext
    XS.modify (\xs -> xs { debugEnableNext = False })
    pure go
  when go $
     ask >>= liftX . debugEventsWindow
  idHook
