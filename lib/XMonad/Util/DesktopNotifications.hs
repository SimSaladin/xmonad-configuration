{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
------------------------------------------------------------------------------
-- |
-- Module      : XMonad.Util.DesktopNotifications
-- Copyright   : (c) Samuli Thomasson, 2019
-- License     : BSD-3
--
-- Maintainer  : Samuli Thomasson <samuli.thomasson@paivola.fi>
-- Stability   : unstable
-- Portability : unportable
--
-- https://developer.gnome.org/notification-spec/
-- https://hackage.haskell.org/package/fdo-notify-0.3.1/docs/DBus-Notify.html#t:Notification
-- https://developer.gnome.org/pango/stable/PangoMarkupFormat.html
--
------------------------------------------------------------------------------

module XMonad.Util.DesktopNotifications (
  module XMonad.Util.DesktopNotifications,
  module DesktopNotifications.Markup,
  module DesktopNotifications.Notifications
  ) where

import qualified XMonad                             as X
import           XMonad                             (Default(..), ExtensionClass(..), MonadIO(liftIO), X)
import           XMonad.Prelude
import qualified XMonad.StackSet                    as W
import qualified XMonad.Util.ExtensibleConf         as XC
import qualified XMonad.Util.ExtensibleState        as XS
import qualified XMonad.Util.NamedWindows           as NW

import           Control.Concurrent                 (forkIO)
import qualified Control.Concurrent.MVar            as MVar
import qualified Control.Exception                  as E

import           Data.Int                           (Int32)
import qualified Data.Map                           as M
import           Data.String                        (IsString(..))
import           Data.Text                          (Text)
import qualified Data.Text                          as T
import qualified Data.Text.Lazy                     as TL
import           Data.Word                          (Word32, Word8)

import           Data.Version                       (showVersion)
import           Text.Printf                        (PrintfArg)

import           System.Directory                   (doesFileExist)
import           System.Timeout                     (timeout)

import           DBus                               hiding (Type)
import qualified DBus.Client                        as C

import           DesktopNotifications.Client
import           DesktopNotifications.Markup
import           DesktopNotifications.Notifications
import           Paths_xmonad_configuration

newtype DesktopNotifications = DesktopNotifications () deriving Semigroup

desktopNotifications :: X.XConfig l -> X.XConfig l
desktopNotifications = XC.once f (DesktopNotifications ())
  where
    f xc = xc { X.startupHook = X.startupHook xc X.<+> startupHook }

data NotificationsClientXS = NotificationsClientXS
  { dbusClient   :: Maybe (MVar.MVar C.Client)
  , lastNotifyId :: NotifyId
  , noteDefaults :: forall t. (Eq t, Monoid t, IsString t) => Note t -> Note t
  }

instance ExtensionClass NotificationsClientXS where
  initialValue = NotificationsClientXS def def id

getAppIcon :: IO String
getAppIcon = getDataFileName "xmonad.svg"

notify :: (Eq t, Monoid t, IsVariant t, IsString t) => Note t -> X NotifyId
notify nt = do
  ndef <- XS.gets noteDefaults
  withDBusClient $ notify' $ ndef nt

notify_ :: (Eq t, Monoid t, IsVariant t, IsString t) => Note t -> X ()
notify_ = void . notify

close :: NotifyId -> X Bool
close = withDBusClient . closeNotification

notifyLast :: Notify -> X ()
notifyLast nf =
  XS.gets lastNotifyId >>= \i ->
  notify (replaces i nf) >>= \i' ->
  XS.modify (\s -> s { lastNotifyId = i' })

notifyLastS :: String -> X ()
notifyLastS = notifyLast . fromString

withDBusClient :: (C.Client -> IO a) -> X a
withDBusClient f = XS.gets dbusClient
  >>= X.io . maybe errNoConn MVar.tryReadMVar
  >>= X.io . maybe errNoConn (timeout (1000000 * 2) . f)
  >>= X.io . maybe errTimeout pure
  where
    errNoConn  = E.throw $ C.clientError "No D-Bus client connection established!"
    errTimeout = E.throw $ C.clientError "Timeout"

-- ** Hooks

-- | Establish Dbus connection.
startupHook :: X ()
startupHook = do
  icon <- X.io getAppIcon
  mvar <- X.io MVar.newEmptyMVar
  void . X.io . forkIO $ connect mvar
  XS.modify $ \s -> s
    { noteDefaults =
      summaryDef "XMonad" .
      appNameDef (T.pack $ "xmonad-" <> showVersion Paths_xmonad_configuration.version) .
      appIconDef icon
    , dbusClient = Just mvar
    }
  where
    connect :: MVar.MVar C.Client -> IO ()
    connect mvar = do
      res <- E.try $ timeout (1000000 * 5) C.connectSession
      either (X.trace . ("Notify: " <>) . show @E.SomeException) (maybe (pure ()) (MVar.putMVar mvar)) res

-- | Tear down existing dbus connection. Useful when restarting xmonad.
exitHook :: X ()
exitHook = XS.gets dbusClient >>= mapM_ disconnect
  where
    disconnect mvar = liftIO $ do
      X.trace "Disconnecting..."
      timeout (1000000 * 10) $ C.disconnect =<< MVar.takeMVar mvar

-- * Urgency hook

urgencyHook :: X.Window -> X ()
urgencyHook = urgencyHookWith $ \tag name ->
  summary (show name) $ body ("requires attention in <b><tt>" ++ tag ++ "</tt></b>") def

urgencyHookWith :: (Eq t, Monoid t, IsVariant t, IsString t) => (X.WorkspaceId -> NW.NamedWindow -> Note t) -> X.Window -> X ()
urgencyHookWith f win = X.withWindowSet $ \wset ->
  X.whenJust (W.findTag win wset) $ \tag -> do
    name <- NW.getName win
    notify_ (f tag name)
