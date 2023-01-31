{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE TypeOperators      #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}

------------------------------------------------------------------------------
-- |
-- Module      : Scratchpads
-- Copyright   : (c) Samuli Thomasson, 2019-2021
-- License     : BSD-3
--
-- Maintainer  : Samuli Thomasson <samuli.thomasson@paivola.fi>
-- Stability   : unstable
-- Portability : unportable
--
------------------------------------------------------------------------------

module Scratchpads
  (
  Scratchpad(..),
  mkPad,
  mkPadDyn,
  exclusive,
  scratchpadsStartupHook,
  managePads,
  -- * Actions
  togglePad,
  toggleScratchpad,
  toggleScratchpad',
  cyclePads,
  scratchpadCompl,
  -- * Dynamic Client Management
  dynUpdateFocusedWindow, dynPadSet, dynPadSet', dynPadCurrent,
  ) where

import           Control.Monad
import qualified Data.List                   as L
import           Data.Semigroup
import           Prelude
import           XMonad
import           XMonad.Actions.Minimize     (maximizeWindowAndFocus, minimizeWindow)
import           XMonad.Hooks.ManageHelpers
import qualified XMonad.Prompt               as XP
import           XMonad.Prompt.ConfirmPrompt (confirmPrompt)
import qualified XMonad.StackSet             as W

import qualified Data.Map                    as M
import           Data.Maybe
import           Text.Printf
import qualified XMonad.Util.ExtensibleState as XS
import           XMonad.Util.NamedCommands
import           XMonad.Util.PureX

--dynamicProp  = "XMONAD_DYNAMIC_TOGGLE"
--dynamicPadId = "dynamic"

type ScratchpadId = String

-- | Custom implementation: affected by NamedScratchpads and ExclusiveScratchpads.
data Scratchpad = SP
  { spName      :: ScratchpadId
  , spCmd       :: X ()
  , spQuery     :: Query Bool
  , spHook      :: ManageHook
  , spExclusive :: [String]
  }

data Scratchpads = Scratchpads { xpads :: M.Map ScratchpadId Scratchpad } deriving Typeable
instance ExtensionClass Scratchpads where
  initialValue = Scratchpads mempty

data ScratchpadDyn = ScratchpadDyn { dynWins :: M.Map ScratchpadId Window } deriving (Typeable, Read, Show)
instance ExtensionClass ScratchpadDyn where
  initialValue = ScratchpadDyn mempty
  extensionType = PersistentExtension

-- * Actions


cyclePads = (? "Toggle next scratchpad (cyclic)") $ do
    padsAll <- XS.gets (M.elems . xpads)
    padsExist <- withWindowSet $ \wset ->
      catMaybes <$> mapM (runQuery xpad) (W.allWindows wset)
    let pads = [pad | pad <- padsAll, any ((==) (spName pad) . spName) padsExist]
    res <- withFocii $ \_ w ->
      fmap (\x -> drop 1 . dropWhile ((/=) (spName x) . spName)) <$> runQuery xpad w
    case fromMaybe id res pads of
      npad:_ -> togglePadNoCreate (spName npad)
      _      -> minimizeScratchpads pads


togglePad name = (? printf "Toggle scratchpad %s" name) $ toggleScratchpad True name
-- | Don't create if missing
togglePadNoCreate = toggleScratchpad False

dynUpdateFocusedWindow :: ScratchpadId -> "Set ad-hoc scratchpad with focused window" :? X ()
dynUpdateFocusedWindow k = cmdT $ dynPadToggleFocused k

dynPadToggleFocused :: ScratchpadId -> X ()
dynPadToggleFocused k = do
    cur <- dynPadCurrent k
    withFocused $ \foc -> dynPadSet k $ if Just foc == cur then Nothing else Just foc

dynPadSet :: ScratchpadId -> Maybe Window -> X ()
dynPadSet k w = do
    old <- dynPadCurrent k
    XS.modify $ \s -> s { dynWins = M.alter (const w) k (dynWins s) }
    forM_ old (toggleWindow (Just True) idHook)
    whenJust w (\_ -> togglePadNoCreate k)

dynPadSet' :: Scratchpad -> Maybe Window -> X ()
dynPadSet' sp w = do
  XS.modify $ \s -> s { xpads = M.alter (const $ Just sp) (spName sp) (xpads s) }
  XS.modify $ \s -> s { dynWins = M.alter (const w) (spName sp) (dynWins s) }

dynPadCurrent :: ScratchpadId -> X (Maybe Window)
dynPadCurrent k = XS.gets $ M.lookup k . dynWins

-- | First, minimize any pads exclusive with target.
-- Then, look for an existing instance in focused workspace.
-- If that fails, look for an instance across all windows.
toggleScratchpad :: Bool -> ScratchpadId -> X ()
toggleScratchpad createIfMissing k = do
    spads <- XS.gets xpads
    whenJust (M.lookup k spads) $ toggleScratchpad' createIfMissing

toggleScratchpad' :: Bool -> Scratchpad -> X ()
toggleScratchpad' createIfMissing sp@SP{spQuery=q,spHook=h} = do
    s <- XS.gets xpads
    case M.lookup (spName sp) s of
      Nothing -> XS.modify $ \s -> s { xpads = M.alter (const $ Just sp) (spName sp) (xpads s) }
      Just _ -> return ()
    toggle [x |x<-M.elems s, spName x `elem` spExclusive sp]
  where
    toggle excl = do
        -- trace ("togglePad: " ++ k ++ " " ++ show (map spName excl))
        minimizeScratchpads excl
        withWindowSet (filterM (runQuery q) . currentWindows) >>= \case
          w : _ -> toggleWindow Nothing h w
          []    -> withWindowSet (filterM (runQuery q) . W.allWindows) >>= \case
              w : _ -> toggleWindow Nothing h w
              []
                | createIfMissing -> spCmd sp
                | otherwise       -> return ()

minimizeScratchpads :: [Scratchpad] -> X ()
minimizeScratchpads xs = withWindowSet $ mapM_ hook . currentWindows
  where
    hook w = runQuery (xpad' xs) w >>= maybe mempty (go w)
    go w sp = toggleWindow (Just False) (spHook sp) w

-- * Hooks

scratchpadsStartupHook :: [Scratchpad] -> X ()
scratchpadsStartupHook pads = do
  mapM_ (flip dynPadSet' Nothing) pads

-- * ManageHooks

managePads :: MaybeManageHook
managePads = xpad >>= flip whenJust' padManageHook

padManageHook :: Scratchpad -> MaybeManageHook
padManageHook sp = spQuery sp -?> spHook sp

xpad :: Query (Maybe Scratchpad)
xpad = liftX (XS.gets xpads) >>= xpad' . M.elems

xpad' :: [Scratchpad] -> Query (Maybe Scratchpad)
xpad' = go where
  go (x:xs) = spQuery x >>= \r -> if r then pure (Just x) else go xs
  go []     = return Nothing


-- * Constructors

-- | "mkPad name mh match launch"
mkPad :: ScratchpadId -> ManageHook -> Query Bool -> X () -> Scratchpad
mkPad nm mh q a = SP nm a q mh []

-- | Make exclusive set.
exclusive :: [Scratchpad] -> [Scratchpad]
exclusive xs = [ x{spExclusive = L.delete (spName x) (map spName xs) } | x <- xs ]

-- | Pad with no static launch action. Use the provided action to bind to an existing window.
mkPadDyn :: ScratchpadId -> XP.XPConfig -> ManageHook -> Scratchpad
mkPadDyn nm xpc mh = mkPad nm mh q a
  where q = liftX (dynPadCurrent nm) >>= \mw -> asks Just =? mw
        a = dynDefaultPrompt xpc nm
        -- a = dynPadToggleFocused nm

-- | Add a launch action to a dynamic pad. It prompts for confirmation to bind focused window to the pad.
dynDefaultPrompt :: XP.XPConfig -> ScratchpadId -> X ()
dynDefaultPrompt xpc k = withFocused $ \foc -> do
    tt <- runQuery title foc
    let text = printf "%s: Bind window %X '%s'" k foc tt
    confirmPrompt xpc text (dynPadSet k (Just foc))

-- * Misc.

currentWindows :: W.StackSet i l a sid sd -> [a]
currentWindows = W.integrate' . W.stack . W.workspace . W.current

-- | Toggle minimize/maximize of a window. See "XMonad.Actions.Minimize"
-- First argument: only maximize (True) or minimize (False).
toggleWindow :: Maybe Bool -> ManageHook -> Window -> X ()
toggleWindow ma mh w = do
  f <- appEndo <$> runQuery mh w
  liftM2 (,) (runQuery isHidden w) inCurrentWS >>= \case
    (True, True)   | ma /= Just False -> modifyWindowSet (f . W.insertUp w . W.delete' w) >> maximizeWindowAndFocus w
    (True, False)  | ma /= Just False -> modifyWindowSet (W.currentTag >>= flip W.shiftWin w) >> windows f >> maximizeWindowAndFocus w
    (False, False) | ma /= Just False -> modifyWindowSet (W.currentTag >>= flip W.shiftWin w) >> windows f
    (False, True)  | ma /= Just True  ->
      do
        windows (W.peek >>= \w' -> if w' == Just w then W.focusUp else id)
        minimizeWindow w
        -- shift to master, so that we won't bump into the minimized window when refocusing after dead window.
        modifyWindowSet (W.peek >>= \w' -> maybe id W.focusWindow w' . W.shiftMaster . W.focusWindow w)
    _ -> return ()
  where
    inCurrentWS = withWindowSet (return . elem w . currentWindows)
    isHidden = isInProperty "_NET_WM_STATE" "_NET_WM_STATE_HIDDEN"


-- * Prompts & completion

-- scratchpadCompl :: [Scratchpad] -> XP.ComplFunction
scratchpadCompl xpc pads = XP.mkComplFunFromList' xpc (map spName pads)
