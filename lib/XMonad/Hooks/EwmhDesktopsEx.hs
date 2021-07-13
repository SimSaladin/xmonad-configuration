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
  setEWMHDesktopGeometry
  ) where

import XMonad

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
