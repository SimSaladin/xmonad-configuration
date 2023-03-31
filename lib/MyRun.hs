{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE PolyKinds                  #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}
------------------------------------------------------------------------------
-- |
-- Module      : MyRun
-- Copyright   : (c) 2019 Samuli Thomasson
-- License     : BSD-3
--
-- Maintainer  : Samuli Thomasson <samuli.thomasson@paivola.fi>
-- Stability   : unstable
-- Portability : non-portable
--
-- "XMonad.Util.Run"
------------------------------------------------------------------------------

module MyRun (
  -- * New
  spawnX,
  spawnProg,
  spawnTerm, TerminalCfg(..),
  spawnUnit, UnitCfg(..),

  -- * Spawn
  Spawn(..),
  Sh,
  sh,

  -- * Commands
  HasCmd(..),

  -- * Execute (don't fork)
  exec,
  readProcess,
  readProcessWithInput,
  runProcess_,

  -- * Spawn (fork)
  spawnPipeIO,

  -- * Misc.
  shell,
  program,

  -- * Exceptions
  RunException(..),

  -- * Systemd
  sdEscape,
  sdRunSpec,
  sdProperty,

  -- * Re-exports
  Posix.ProcessID,
  IO.Handle,
  IO.BufferMode(..),
  IO.hClose,
  IO.hPutStrLn,
  IO.hSetBuffering,
  IO.stdout,
  IO.stderr,
  P.CmdSpec,
  ) where

import           XMonad                   hiding (spawn)
import           XMonad.Util.Run          (seconds)

import           Prelude

import           Control.Concurrent (myThreadId, killThread, mkWeakThreadId)
import qualified Control.Exception        as E
import           Control.Monad
import qualified Data.Map                 as M
import           Data.String              (IsString)
import qualified System.IO                as IO
import qualified System.Posix             as Posix
import qualified System.Posix.Signals             as Signals
import           GHC.Weak                 (deRefWeak)
import qualified System.Process           as P
import           System.Timeout           (timeout)
import           Text.Printf              (printf)

import           Codec.Binary.UTF8.String (encodeString)

import RawFilePath

-- X.Core.spawn          - void (spawnPID ...)
-- X.Core.spawnPID       - xfork (executeFile ...)
-- X.Core.xfork          - forkProcess w/ signal handler handling
-- X.U.Run.safeSpawn     - exec
-- X.U.Run.safeSpawnProg - exec (no arguments)
-- X.U.Run.unsafeSpawn   - exec /bin/sh -c
-- X.U.Run.spawnPipe     - exec /bin/sh -c (return handle for pipe for stdin)

-- SHELL

newtype Sh = Sh { unSh :: String }
  deriving stock (Show, Read)
  deriving newtype (IsString)

sh :: String -> Sh
sh = Sh

-- CLASS

-- | Things that are essentially executable commands ("CmdSpec").
class HasCmd f a where
  cmdSpec :: a -> f P.CmdSpec

-- | The trivial (identity) instance: A "CmdSpec" has a "CmdSpec" (itself).
instance Applicative f => HasCmd f P.CmdSpec where
  cmdSpec = pure

-- | Shell commands as strings. For convenience with -XOverloadedStrings.
instance Applicative f => HasCmd f Sh where
  cmdSpec = pure . P.ShellCommand . unSh

-- | The FilePath (String) instance will execute the file directly.
instance Applicative f => HasCmd f FilePath where
  cmdSpec = pure . (`P.RawCommand` [])

-- | The (file, args) tuple instance will execute the file with the arguments given.
instance Applicative f => HasCmd f (FilePath, [String]) where
  cmdSpec = pure . uncurry P.RawCommand

-- | The @(SdRunSpec, cmd)@ instance will launch the command with systemd-run.
instance HasCmd X a => HasCmd X (SdRunSpec, a) where
  cmdSpec (spec, cmd) = do
    cmd' <- cmdSpec cmd
    sdRunCmd spec cmd'

-- | The class that drives the polyvariadic @spawn@ function.
--
-- @
-- spawn "chromium"
-- spawn (shell "mpv $(xclip -o)")
-- spawn "mpc" ["toggle"]
-- @
--
class Spawn a where
  spawn :: a

-- | Spawn a process.
instance (MonadIO m, HasCmd m a) => Spawn (a -> m ()) where
  spawn = spawn >=> \(_::Posix.ProcessID) -> return ()

-- | Spawn a process returning its PID.
instance (MonadIO m, HasCmd m a) => Spawn (a -> m Posix.ProcessID) where
  spawn = cmdSpec >=> \cs -> logCmdInfo "spawn" cs >> xfork (exec cs)

-- | Spawn a process from two arguments.
instance (MonadIO m, HasCmd m (a,b)) => Spawn (a -> b -> m ()) where
  spawn = curry spawn

-- | Spawn a process from three arguments.
instance (MonadIO m, HasCmd m (a,(b,c))) => Spawn (a -> b -> c -> m ()) where
  spawn = flip $ flip . curry (flip (curry spawn))

-- Functions

-- | System.Posix.executeFile
exec :: HasCmd IO cmd => cmd -> IO ()
exec cmd = cmdSpec cmd >>= exec' where
  exec' (P.ShellCommand script)  = Posix.executeFile "/bin/sh" False ["-c", encodeString script] Nothing
  exec' (P.RawCommand prog args) = Posix.executeFile (encodeString prog) True (map encodeString args) Nothing

data TerminalCfg = TerminalCfg
  { terminalName      :: String
  , terminalGeometry  :: String
  , terminalHold      :: Bool
  , terminalSaveLines :: Int
  , terminalInstanceId :: String
  , terminalProg      :: Maybe ([String] -> P.CmdSpec)
  }

instance Default TerminalCfg where
  def = TerminalCfg "" "" False (-1) "" Nothing

spawnTerm :: (HasCmd X cmd, Spawn (P.CmdSpec -> X r)) => TerminalCfg -> cmd -> X r
spawnTerm tcfg cmd = do
  termCmd <- cmdSpec cmd
  case terminalProg tcfg of
    Just go -> spawn $ go $ concat [ prog : args | (prog,args) <- [showCmdSpec termCmd]]
    Nothing   -> do
      tprog <- asks (terminal . config)
      spawn $ program tprog $ opts termCmd
    where
      opts cmd' = concat $
        [["-name", name] | name <- [terminalName tcfg], name /= ""] ++
        [["-geometry", val] | val <- [terminalGeometry tcfg], val /= ""] ++
        [["-hold"] | terminalHold tcfg] ++
        [["-sl", show val] | val <- [terminalSaveLines tcfg], val >= 0] ++
        [["-instance-id", val] | val <- [terminalInstanceId tcfg], val /= ""] ++
        ["-e" : prog : args | (prog,args) <- [showCmdSpec cmd'], prog /= ""]

data UnitCfg = UnitCfg

instance Default UnitCfg where def = UnitCfg

spawnUnit :: (MonadIO m, HasCmd m cmd) => UnitCfg -> cmd -> m ()
spawnUnit _ucfg cmd = do
  (prog, args) <- showCmdSpec <$> cmdSpec cmd
  spawnProg "systemd-run" $ opts ++ ["--", prog] ++ args
    where
      opts = ["--quiet", "--user", "--no-block", "--no-ask-password", "--collect", "--send-sighup"]

spawnX :: (HasCmd X cmd) => cmd -> X ()
spawnX cmd = do
  (prog, args) <- showCmdSpec <$> cmdSpec cmd
  spawnProg prog args

spawnProg :: MonadIO m => String -> [String] -> m ()
spawnProg prog args = spawn $ program prog args

-- SYSTEMD-RUN

data SdRunSpec = SdRunSpec
  { unitName       :: !String -- ^ The foo in foo.service
  , unitScope      :: !Bool
  , unitSlice      :: !(Maybe String)
  , unitInstance   :: !String -- ^ bar in foo@bar.service
  , unitIsTemplate :: !Bool   -- ^ Is template unit
  , unitProperties :: !(M.Map String String) -- ^ --property=NAME=VALUE
  } deriving (Show, Read)

-- | @sdRunSpec unit instance@
sdRunSpec :: String -> String -> SdRunSpec
sdRunSpec u i = SdRunSpec
  { unitName       = u
  , unitScope      = False
  , unitSlice      = Nothing
  , unitInstance   = i
  , unitIsTemplate = not (null i)
  , unitProperties = mempty
  }

-- | @sdProperty "LimitNOFILE" "16384"@
sdProperty :: String -> String -> SdRunSpec -> SdRunSpec
sdProperty p v spec = spec { unitProperties = M.insert p v (unitProperties spec) }

sdRunCmd :: MonadIO m => SdRunSpec -> P.CmdSpec -> m P.CmdSpec
sdRunCmd SdRunSpec{..} cmd = do

  unit <- case unitName of
    "" -> return []
    _  -> sdEscape
      (if unitIsTemplate then Just (unitName <> "@.service") else Nothing)
      [if unitIsTemplate then unitInstance else unitName <> ".service"]

  return $ P.RawCommand "systemd-run" $
      ["--quiet","--user","--no-block","--no-ask-password", "--collect", "--send-sighup"] <>
      ["--scope" | unitScope] <>
      ["--unit=" <> unit' | unit' <- take 1 unit] <>
      ["--slice=" <> slice | Just slice <- [unitSlice]] <>
      ["--property=" <> property <> "=" <> value | (property, value) <- M.toList unitProperties] <>
      ("--" : uncurry (:) (showCmdSpec cmd))


-- PRIMITIVES

data RunException = RunProcessTimeout deriving (Eq, Show)
instance E.Exception RunException

program :: FilePath -> [String] -> P.CmdSpec
program = P.RawCommand

shell :: String -> P.CmdSpec
shell = P.ShellCommand

showCmdSpec :: P.CmdSpec -> (String, [String])
showCmdSpec (P.RawCommand prog args) = (prog, args)
showCmdSpec (P.ShellCommand cmd)     = ("/bin/sh", ["-c", cmd])

-- | Run external command and read its stdout.
readProcess :: MonadIO m => FilePath -> [String] -> m String
readProcess prog args = runProcess_ (P.RawCommand prog args) Nothing

readProcessWithInput :: MonadIO m => FilePath -> [String] -> String -> m String
readProcessWithInput prog args = runProcess_ (P.RawCommand prog args) . Just

runProcess_ :: MonadIO m => P.CmdSpec -> Maybe String -> m String
runProcess_ cspec minput = do
  logCmdInfo "run" cspec

  let (prog, args) = showCmdSpec cspec
  let cp = (P.proc prog args) { P.std_in  = maybe P.NoStream (const P.CreatePipe) minput
                              , P.std_out = P.CreatePipe
                              , P.std_err = P.Inherit
                              }

  result <- io $ timeout (seconds 2) $ P.withCreateProcess cp $ \hin hout _ _ -> do
      whenJust minput $ \input ->
        whenJust hin $ \h ->
          IO.hPutStr h input >> IO.hClose h
      output <- maybe undefined IO.hGetContents hout
      length output `seq` return output

  io $ maybe (E.throwIO RunProcessTimeout) return result

-- | Like spawnPipe but not interpreting as shell
spawnPipeIO :: MonadIO m => IO () -> m (IO.Handle, Posix.ProcessID)
spawnPipeIO x = io $ do
    (rd, wd) <- Posix.createPipe
    Posix.setFdOption wd Posix.CloseOnExec True
    wh <- Posix.fdToHandle wd
    IO.hSetEncoding wh IO.utf8
    IO.hSetBuffering wh IO.LineBuffering

    cpid <- xfork $ do
      Posix.dupTo rd Posix.stdInput
      ---- handle SIGTERM in the RTS
      --runThreadIdWk <- mkWeakThreadId =<< myThreadId
      --void $ Signals.installHandler
      --  Signals.sigTERM
      --  (Signals.CatchOnce $ do
      --    runThreadIdMay <- deRefWeak runThreadIdWk
      --    case runThreadIdMay of
      --      Nothing -> return ()
      --      Just runThreadId -> killThread runThreadId
      --  )
      --  Nothing
      x
    Posix.closeFd rd
    return (wh, cpid)

-- spawnPipeIO2 :: MonadIO m => IO () -> m (IO.Handle, Posix.ProcessID)

-- LOGGING

logCmdInfo :: MonadIO m => String -> P.CmdSpec -> m ()
logCmdInfo s cmd = trace $ printf "%s: %s" s (uncurry P.showCommandForUser (showCmdSpec cmd))

-- SYSTEMD-ESCAPE

-- | systemd-escape
-- @ sdEscape (Just "foo@.service") ["bar/"] == ["foo@bar-.service"] @
sdEscape :: MonadIO m
         => Maybe String -- ^ Template (foo@.service)
         -> [String] -- ^ Strings to escape
         -> m [String]
sdEscape template args = liftIO $ lines <$> readProcess "systemd-escape" opts
  where
    opts = ["--template=" <> s | Just s <- [template]] ++ ("--" : args)
