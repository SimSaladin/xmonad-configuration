------------------------------------------------------------------------------
-- |
-- Module      : XMonad.Hooks.EwmhDesktopsEx
-- Description : Custom extensions for EWMH support
-- Copyright   : (c) Samuli Thomasson, 2020
-- License     : WTFPL
--
-- Maintainer  : Samuli Thomasson <samuli.thomasson@paivola.fi>
-- Stability   : unstable
-- Portability : unportable
--
------------------------------------------------------------------------------

module XMonad.Hooks.EwmhDesktopsEx (
  setEWMHDesktopGeometry,
  myFullscreenEventHook
  ) where

import qualified Data.List
import           XMonad
import           XMonad.Prelude
import qualified XMonad.StackSet              as W
import           XMonad.Util.WindowProperties

-- | A startup hook that sets _NET_DESKTOP_GEOMETRY on the root window.
-- See https://github.com/xmonad/xmonad-contrib/issues/150#issuecomment-292828640
setEWMHDesktopGeometry :: X ()
setEWMHDesktopGeometry = withDisplay $ \dpy -> do
    wm <- asks theRoot
    cardinalType <- getAtom "CARDINAL"
    desktopGeometryProp <- getAtom "_NET_DESKTOP_GEOMETRY"
    io $ do
        windowAttributes <- getWindowAttributes dpy wm
        let width = fromIntegral $ wa_width windowAttributes
            height = fromIntegral $ wa_height windowAttributes
        changeProperty32 dpy wm desktopGeometryProp cardinalType propModeReplace
                         [width, height]

-- | Modified from H.EWMH.fullscreenEventHook to refresh on sink.
myFullscreenEventHook :: Event -> X All
myFullscreenEventHook (ClientMessageEvent _ _ _ dpy win typ (action:dats)) = do
  managed <- isClient win
  wmstate <- getAtom "_NET_WM_STATE"
  fullsc <- getAtom "_NET_WM_STATE_FULLSCREEN"
  wstate <- fromMaybe [] `fmap` getProp32 wmstate win

  let isFull = fromIntegral fullsc `elem` wstate

      -- Constants for the _NET_WM_STATE protocol:
      remove = 0
      add = 1
      toggle = 2
      chWstate f = io $ changeProperty32 dpy win wmstate aTOM propModeReplace (f wstate)

  when (managed && typ == wmstate && fi fullsc `elem` dats) $ do
    when (action == add || (action == toggle && not isFull)) $ do
      chWstate (fi fullsc:)
      windows $ W.float win $ W.RationalRect 0 0 1 1
    when (action == remove || (action == toggle && isFull)) $ do
      chWstate $ Data.List.delete (fi fullsc)
      windows $ W.sink win
      refresh -- XXX: added

  return $ All True

myFullscreenEventHook _ = return $ All True
