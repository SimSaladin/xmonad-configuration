
------------------------------------------------------------------------------
-- |
-- Module      : DesktopEntries
-- Description : Short description
-- Copyright   : (c) Samuli Thomasson, 2020
-- License     : WTFPL
--
-- Maintainer  : Samuli Thomasson <samuli.thomasson@paivola.fi>
-- Stability   : unstable
-- Portability : unportable
--
-- Interface with XDG desktop entries.
--
-- Interesting attributes: Name, Exec, Terminal, Categories, Comment, MimeType
--
-- xdg-desktop-menu install ./shinythings-webmirror.desktop
--
-- xdg-settings get default-web-browser
-- xdg-settings get default-url-scheme-handler zoommtg
--
-- gtk-launch app-name.desktop [URI...]
------------------------------------------------------------------------------

module DesktopEntries where

import XMonad (X, io, trace, MonadIO)
import XMonad.Prompt
import           System.Environment.XDG.DesktopEntry -- package xdg-desktop-entry
import           System.FilePath
import MyRun

data DesktopEntryPrompt = DesktopEntryPrompt
instance XPrompt DesktopEntryPrompt where
  showXPrompt DesktopEntryPrompt{} = "Desktop Entry: "
  nextCompletion     _ = getNextCompletion
  commandToComplete  _ = id

desktopEntryPrompt :: XPConfig -> [String] -> X ()
desktopEntryPrompt xpconfig targets = do
  deEntries <- filter (not . deNoDisplay) <$> io getDirectoryEntriesDefault
  let deNames = deName [] <$> deEntries
  let comps = zip deNames deEntries
  mkXPrompt DesktopEntryPrompt xpconfig (mkComplFunFromList' xpconfig deNames)
    (mapM_ (`launchDesktopEntry'` targets) . flip lookup comps)

launchDesktopEntry :: MonadIO m => FilePath -> [String] -> m ()
launchDesktopEntry name targets =
  io (getDirectoryEntryDefault name) >>=
    maybe (trace ("launchDesktopEntry: name not found: " ++ name)) (`launchDesktopEntry'` targets)

launchDesktopEntry' :: MonadIO m => DesktopEntry -> [String] -> m ()
launchDesktopEntry' entry targets =
  spawn $ program "gtk-launch" (takeBaseName (deFilename entry) : targets)
