{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}
------------------------------------------------------------------------------
-- |
-- Module      : DesktopNotifications.Client
-- Description : Desktop notifications client
-- Copyright   : (c) Samuli Thomasson, 2024
-- License     : WTFPL
--
-- Maintainer  : Samuli Thomasson <samuli.thomasson@paivola.fi>
-- Stability   : unstable
-- Portability : unportable
--
-- Longer description of this module.
--
------------------------------------------------------------------------------
module DesktopNotifications.Client where

import qualified Control.Exception                  as E
import           Data.Word                          (Word32, Word8)
import           DBus                               hiding (Type)
import qualified DBus.Client                        as C
import qualified System.IO

import           DesktopNotifications.Notifications

-- * Constants

object :: ObjectPath
object = "/org/freedesktop/Notifications"

interface :: InterfaceName
interface = "org.freedesktop.Notifications"

busname :: BusName
busname = "org.freedesktop.Notifications"

-- * Methods

notifyMethod :: MemberName -> MethodCall
notifyMethod name = (methodCall object interface name) { methodCallDestination = Just busname }

-- ** Notify

notify' :: IsVariant t => Note t -> C.Client -> IO NotifyId
notify' nt c = C.call_ c (notifyMethod "Notify") { methodCallBody = toVariants nt } >>= \r ->
  case fromVariant <$> methodReturnBody r of
    [Just v] -> pure v
    _        -> E.throw (C.clientError $ "unexpected response: " ++ show r)

-- ** CloseNotification

closeNotification :: NotifyId -> C.Client -> IO Bool
closeNotification nid c =
  C.call_ c (notifyMethod "CloseNotification"){ methodCallBody = [toVariant nid] } >>= \r ->
    case fromVariant <$> methodReturnBody r of
      [Just v] -> return v
      _        -> E.throw (C.clientError $ "unexpected response: " ++ show r)

-- ** GetCapabilities

getCapabilities :: C.Client -> IO [String]
getCapabilities c =
  C.call_ c (notifyMethod "GetCapabilities") >>= \r ->
    case fromVariant `mapM` methodReturnBody r of
      Just caps -> return caps
      _         -> E.throw (C.clientError $ "unexpected response: " ++ show r)

-- ** GetServerInformation

data ServerInformation = ServerInformation
  { serverName    :: String
  , serverVendor  :: String
  , serverVersion :: String
  } deriving (Eq, Show, Read)

getServerInformation :: C.Client -> IO ServerInformation
getServerInformation c =
  C.call_ c (notifyMethod "GetServerInformation") >>= \r ->
    case fromVariant <$> methodReturnBody r of
      [Just nm, Just vr, Just v] -> return $! ServerInformation nm vr v
      vs                         -> E.throw (C.clientError $ "unexpected response: " ++ show vs)

-- * Signals

-- ** NotifyClosed

onNotifyClosed :: C.Client -> (NotifyId -> CloseReason -> IO ()) -> IO C.SignalHandler
onNotifyClosed client go =
  addHandler client "NotifyClosed" $ \case
    [vid, vr] | Just i <- fromVariant vid, Just r <- fromVariant vr -> go i r
    other -> System.IO.hPutStrLn System.IO.stderr ("onNotifyClosed: unexpected signal body type: " ++ show other)

data CloseReason = Expired
                 | UserDismissed
                 | ByMethodCall
                 | Unspecified
                 deriving (Eq, Show, Read, Enum, Bounded)

instance IsVariant CloseReason where
  toVariant   = toVariant @Word32 . toEnum . fromEnum
  fromVariant = fmap (toEnum . fromEnum @Word32) . fromVariant

-- ** ActionInvoked

onActionInvoked :: C.Client -> (NotifyId -> String -> IO ()) -> IO C.SignalHandler
onActionInvoked client go =
  addHandler client "ActionInvoked" $ \case
    [vid, vr] | Just i <- fromVariant vid, Just r <- fromVariant vr -> go i r
    other -> System.IO.hPutStrLn System.IO.stderr ("onActionInvoked: unexpected signal body type: " ++ show other)

-- ** Internal

addHandler :: C.Client -> MemberName -> ([Variant] -> IO ()) -> IO C.SignalHandler
addHandler client member go = C.addMatch client rule (go . signalBody)
  where rule = C.matchAny { C.matchInterface = Just interface, C.matchMember = Just member }
