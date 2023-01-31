{-# LANGUAGE ScopedTypeVariables #-}
------------------------------------------------------------------------------
-- |
-- Module      : XMonad.Hooks.NamedLoggers
-- Description : Short description
-- Copyright   : (c) Samuli Thomasson, 2023
-- License     : WTFPL
--
-- Maintainer  : Samuli Thomasson <samuli.thomasson@paivola.fi>
-- Stability   : unstable
-- Portability : unportable
--
-- Longer description of this module.
--
------------------------------------------------------------------------------
module XMonad.Hooks.NamedLoggers
  ( NamedLoggerId(..)
  , namedLoggersLogHook
  , withNamedLogInputs
  , cleanupNamedLoggers
  ) where

import XMonad
import XMonad.Prelude
import qualified Data.Map                             as Map
import           Data.IORef
import qualified System.Posix                         as Posix
import qualified System.IO                            as IO
import           XMonad.Util.Loggers
import qualified XMonad.Util.ExtensibleState          as XS
import qualified Control.Exception                    as E
import qualified Data.Set as Set
import           XMonad.Hooks.StatusBar.PP            (PP(..), dynamicLogString, pad, shorten, wrap, xmobarRaw)
import           System.IO.Unsafe                     (unsafePerformIO)
import           System.Timeout                       (timeout)
import           XMonad.Util.PureX
import           Codec.Binary.UTF8.String             (encodeString)

import MyRun (Handle)

data NamedLoggerId = NLogTitle | NLogLayout
  deriving (Show, Read, Eq, Ord, Enum, Bounded)

newtype NamedLoggersXS = NamedLoggersXS { nlLasts :: Map.Map (NamedLoggerId, ScreenId) String }
instance ExtensionClass NamedLoggersXS where
  initialValue = NamedLoggersXS mempty

type NamedLoggers = Map.Map NamedLoggerId (Map.Map ScreenId (Maybe Handle, Maybe Handle)) -- (focused, unfocused)

namedLoggersRef :: IORef NamedLoggers
namedLoggersRef = unsafePerformIO (newIORef mempty)
{-# NOINLINE namedLoggersRef #-}

withNamedLogInputs :: ScreenId -> (Map.Map NamedLoggerId Posix.Fd -> IO a) -> IO a
withNamedLogInputs screen = E.bracket
  (Map.fromList <$> mapM (mkLogFd screen) [minBound..maxBound])
  cleanup
    where
      cleanup :: Map.Map NamedLoggerId Posix.Fd -> IO ()
      cleanup = mapM_ $ catchIO . Posix.closeFd

mkLogFd :: ScreenId -> NamedLoggerId -> IO (NamedLoggerId, Posix.Fd)
mkLogFd s k = do
  (rd,wd) <- Posix.createPipe
  Posix.setFdOption wd Posix.CloseOnExec True
  wh <- Posix.fdToHandle wd
  IO.hSetBuffering wh IO.LineBuffering
  addLogListener k s (Just wh, Nothing)
  pure (k,rd)

addLogListener :: NamedLoggerId -> ScreenId -> (Maybe Handle, Maybe Handle) -> IO ()
addLogListener k s hs = atomicModifyIORef namedLoggersRef $ \m -> (Map.insertWith (<>) k (Map.singleton s hs) m, ())

cleanupNamedLoggers :: MonadIO m => m ()
cleanupNamedLoggers = io $ do
    trace "Cleaning up named loggers..."
    logxs <- readIORef namedLoggersRef
    forM_ [ (k,Map.keys a) | (k,a) <- Map.toList logxs ] $ uncurry closeNamedLoggers
    trace "Named loggers cleaned up."

closeNamedLoggers :: MonadIO m => NamedLoggerId -> [ScreenId] -> m ()
closeNamedLoggers k [] = return ()
closeNamedLoggers k hs = io $ do
  rs <- atomicModifyIORef namedLoggersRef $ \m ->
    let (r, m') = Map.updateLookupWithKey (\_ a -> Just $ Map.withoutKeys a (Set.fromList hs)) k m
     in (m', r)
  forM_ (catMaybes $ Map.elems (fromMaybe mempty rs) >>= \(a,b) -> [a,b]) (catchIO . IO.hClose)

-- * Loggers

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
      Map.traverseWithKey (writeBoth s) slogm >>= closeNamedLoggers k . catMaybes . Map.elems
  where
    writeBoth :: MonadIO m => ScreenId -> ScreenId -> (Maybe Handle, Maybe Handle) -> m (Maybe ScreenId) -- (Either String ())
    writeBoth s s' (mf, mu)
      | Just h <- if s == s' then mf else mu = io $ do
        r <- timeout 100000 $ E.catch (fmap Right $ IO.hPutStrLn h $ encodeString str) $ \(ex :: E.IOException) -> pure (Left ex)
        case r of
          Nothing         -> trace "namedLogString: timeout reached" >> return Nothing
          Just (Left ex)  -> trace ("namedLogString: " ++ E.displayException ex) >> return (Just s')
          Just (Right ()) -> return Nothing
      | otherwise = return Nothing

namedLoggersLogHook :: PP -> X ()
namedLoggersLogHook pp = do
  namedLogLazy NLogTitle  (ppTitle pp  `onLogger` logTitle)
  namedLogLazy NLogLayout logLayout
