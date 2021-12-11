{-# LANGUAGE TypeApplications #-}
------------------------------------------------------------------------------
-- |
-- Module      : SpawnOnByPPID
-- Description : Short description
-- Copyright   : (c) Samuli Thomasson, 2020
-- License     : WTFPL
--
-- Maintainer  : Samuli Thomasson <samuli.thomasson@paivola.fi>
-- Stability   : unstable
-- Portability : unportable
--
-- For use with XMonad.Actions.SpawnOn
--
------------------------------------------------------------------------------

module SpawnOnByPPID where

import           MyRun
import           XMonad
import qualified XMonad.Actions.SpawnOn     as SpawnOn
import           XMonad.Hooks.ManageHelpers
import           XMonad.Prelude
import qualified XMonad.Prompt              as XP
import           XMonad.Util.PureX

data PIDPrompt = PIDPrompt

instance XP.XPrompt PIDPrompt where
  showXPrompt PIDPrompt = "PID> "
  nextCompletion _ = \s -> XP.getNextCompletion s . map (unwords . take 1 . words)
  commandToComplete _ = id
  completionToCommand _ = unwords . take 1 . words

pidPrompt :: XP.XPConfig -> String -> X (Maybe ProcessID)
pidPrompt xpconfig _name = do
  cf <- getPIDCompl xpconfig
  XP.mkXPromptWithReturn PIDPrompt xpconfig{ XP.sorter = flip const } cf
    (return . fromIntegral @Int . read . takeWhile isNumber . dropWhile (not.isNumber))

getPIDCompl :: XP.XPConfig -> X XP.ComplFunction
getPIDCompl xpconfig = do
  windowPID <- fromMaybe (-1) . getAlt <$> withFocii (\_ w -> Alt <$> runQuery pid w)
  ps        <- lines <$> readProcess "ps" ["-o", "pid,ppid,pgid,args", "--no-headers", "-eH"]
  let ps' = [ln ++ extra | ln <- ps, pid_:_ppid:pgid:_args <- [words ln], let extra = if show (toInteger windowPID) `elem` [pgid,pid_] then " [FOCUSED]" else ""]
  return $ \s ->
    return $ go [pid_ | ln<-ps', XP.searchPredicate xpconfig s ln, pid_:_<-[words ln]] ps' s
  where
    go incl (x:xs) s
      | pid_:ppid:pgid:_ <- words x, pid_ `elem` incl || ppid `elem` incl || pgid `elem` incl = x : go incl xs s
      | otherwise                                                        = go incl xs s
    go _ [] _ = []

setManageByPPID :: ProcessID -> WorkspaceId -> X ()
setManageByPPID pPID ws = SpawnOn.manageByPPID pPID $
  if null ws then idHook else doShift ws
