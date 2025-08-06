
------------------------------------------------------------------------------
-- |
-- Module      : XMonad.Prompt.Environ
-- Description : Prompts to interact with environment variables of the XMonad process
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

module XMonad.Prompt.Environ (environPrompt) where

import           XMonad

import qualified XMonad.Prompt               as XP
import           XMonad.Prompt.ConfirmPrompt (confirmPrompt)
import qualified XMonad.Prompt.Input         as XP.Input

import qualified Data.List                   as L
import           Data.Maybe
import qualified System.Environment
import           Text.Printf                 (printf)

import qualified MyRun

environPrompt :: XP.XPConfig -> X ()
environPrompt xpc = do
  varsEnv <- io System.Environment.getEnvironment
  varsSessionString <- MyRun.readProcess "systemctl" [ "--user", "--no-block", "--no-pager", "show-environment" ]

  let varsSession = map parseLine $ lines varsSessionString
      vars = varsSession ++ [ entry | entry@(k, _) <- varsEnv, k `notElem` map fst varsSession ]

  XP.Input.inputPromptWithCompl xpc "Environment"
    (\s -> return $ filter (XP.searchPredicate xpc s) [ k <> "=" <> v | (k,v) <- vars ])
    XP.Input.?+ setsEnvCmd
  where
    setsEnvCmd :: String -> X ()
    setsEnvCmd input
      | (k,_:v) <- span (/= '=') input = do
        io (System.Environment.lookupEnv k) >>= \c ->
          confirmPrompt xpc (printf "set %s='%s' (current '%s')" k v (fromMaybe "" c)) (io (System.Environment.setEnv k v))
        confirmPrompt xpc
          (printf "Set %s in systemd user manager environment" k)
          (io . MyRun.exec $ MyRun.program "systemctl" [ "--user", "--no-block", "--no-ask-password", "set-environment", k <> "=" <> v ])
      | otherwise = return ()

parseLine :: String -> (String, String)
parseLine line = let (key, rest) = L.break (== '=') line
                     in case rest of
                          ('=':val) -> (key, val)
                          _         -> (key, "")  -- fallback in case there's no '='
