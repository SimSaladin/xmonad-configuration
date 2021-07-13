
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

import XMonad

import qualified XMonad.Prompt                       as XP
import           XMonad.Prompt.ConfirmPrompt         (confirmPrompt)
import qualified XMonad.Prompt.Input                 as XP.Input

import           Data.Maybe
import           Text.Printf                         (printf)
import qualified System.Environment

environPrompt :: XP.XPConfig -> X ()
environPrompt xpc = do
  vars <- io System.Environment.getEnvironment
  XP.Input.inputPromptWithCompl xpc "Environment"
    (\s -> return $ filter (XP.searchPredicate xpc s) [k<>"="<>v|(k,v)<-vars])
    XP.Input.?+ setsEnvCmd
  where
    setsEnvCmd :: String -> X ()
    setsEnvCmd input
      | (k,_:v) <- span (/= '=') input =
        io (System.Environment.lookupEnv k) >>= \c ->
          confirmPrompt xpc (printf "set %s='%s' (current '%s')" k v (fromMaybe "" c)) (io (System.Environment.setEnv k v))
      | otherwise = return ()
