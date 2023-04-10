{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}


------------------------------------------------------------------------------
-- |
-- Module      : XMonad.Util.NamedCommands.Orphans
-- Description : Orphan instances
-- Copyright   : (c) Samuli Thomasson, 2020
-- License     : WTFPL
--
-- Maintainer  : Samuli Thomasson <samuli.thomasson@paivola.fi>
-- Stability   : unstable
-- Portability : unportable
--
--
--
------------------------------------------------------------------------------

module XMonad.Util.NamedCommands.Orphans where

import           XMonad
import qualified XMonad.Util.NamedActions           as NA
import           XMonad.Util.NamedCommands

import           XMonad.Actions.OnScreen            (Focus(..))
import           XMonad.Actions.PhysicalScreens     (PhysicalScreen(..))
import           XMonad.Hooks.ManageDocks           (ToggleStruts(..))
import qualified XMonad.Layout.BinarySpacePartition as BSP
import qualified XMonad.Layout.GridVariants         as GV
import           XMonad.Layout.Magnifier            (MagnifyMsg)
import qualified XMonad.Layout.Magnifier            as Magnifier (MagnifyMsg(..))
import qualified XMonad.Layout.Mosaic               as Mosaic
import           XMonad.Layout.MultiToggle          (Toggle(Toggle), Transformer)
import qualified XMonad.Layout.WindowArranger       as WA
import           XMonad.Layout.WindowNavigation     (Navigate(Apply, Go, Move, Swap))
import qualified XMonad.Prompt                      as XP

instance IsCmd Navigate where
  command msg = NA.NamedAction (sendMessage msg, [describe' msg]) where
    describe' (Go d2)       = "WinNav: Go "       <> show d2
    describe' (Swap d2)     = "WinNav: Swap "     <> show d2
    describe' (Move d2)     = "WinNav: Move "     <> show d2
    describe' (Apply _f d2) = "WinNav: Apply f? " <> show d2

  cmdEnum _ = [f d2 | d2 <- enumConstrs, f <- [Go, Swap, Move]]

newtype Toggle' t a = Toggle' t

instance (Show t, Typeable t, Transformer t a, Typeable a) => IsCmd (Toggle' t a) where
  command (Toggle' t) = NA.NamedAction (sendMessage (Toggle t), [desc]) where
    desc = "Toggle " <> show t <> " (" <> show (typeOf t) <> ")"

  cmdEnum _ = [] -- TODO

instance Show MagnifyMsg where
  show Magnifier.MagnifyLess = "Magnify (less)"
  show Magnifier.MagnifyMore = "Magnify (more)"
  show Magnifier.Toggle      = "Magnify (toggle)"
  show Magnifier.ToggleOff   = "Magnify (off)"
  show Magnifier.ToggleOn    = "Magnify (on)"

deriving instance Show BSP.Rotate
deriving instance Show BSP.Swap
deriving instance Show BSP.ResizeDirectional
deriving instance Show BSP.TreeRotate
deriving instance Show BSP.TreeBalance
deriving instance Show BSP.FocusParent
deriving instance Show BSP.SelectMoveNode
deriving instance Show BSP.SplitShiftDirectional

deriving instance Data MagnifyMsg
deriving instance Data ChangeLayout
deriving instance Data Resize
deriving instance Data IncMasterN
deriving instance Data ToggleStruts
deriving instance Data XP.Direction1D
deriving instance Data BSP.Direction2D
deriving instance Data BSP.Rotate
deriving instance Data BSP.Swap
deriving instance Data BSP.ResizeDirectional
deriving instance Data BSP.TreeRotate
deriving instance Data BSP.TreeBalance
deriving instance Data BSP.FocusParent
deriving instance Data BSP.SelectMoveNode
deriving instance Data BSP.SplitShiftDirectional

deriving instance Show WA.WindowArrangerMsg

instance IsCmd WA.WindowArrangerMsg where
  command msg = sendMessage msg ? (show msg <> " (WA)")
  cmdEnum _ = [WA.Arrange, WA.DeArrange] ++
    [ WA.IncreaseLeft  i
    , WA.IncreaseRight i
    , WA.IncreaseDown  i
    , WA.IncreaseUp    i
    , WA.DecreaseLeft  i
    , WA.DecreaseRight i
    , WA.DecreaseDown  i
    , WA.DecreaseUp    i
    ] where i = 25

instance IsCmd Mosaic.Aspect where
  command x@Mosaic.Wider      = sendMessage x ? "MOSAIC Wider"
  command x@Mosaic.Taller     = sendMessage x ? "MOSAIC Taller"
  command x@Mosaic.Reset      = sendMessage x ? "MOSAIC Reset"
  command x@Mosaic.SlopeMod{} = sendMessage x ? "MOSAIC Slope ..."
  cmdEnum _ = [Mosaic.Taller, Mosaic.Wider, Mosaic.Reset]

deriving instance Data PhysicalScreen

deriving instance Show GV.ChangeGridGeom

instance IsCmd GV.ChangeGridGeom where
  cmdEnum _ = [GV.SetGridAspect    r | r <- []] ++ [GV.ChangeGridAspect r | r <- [2/100,-2/100]]

deriving instance Show GV.ChangeMasterGridGeom

instance IsCmd GV.ChangeMasterGridGeom where
  cmdEnum _ = concat
    [[GV.IncMasterRows i | i <- [1,-1]]
    ,[GV.IncMasterCols i | i <- [1,-1]]
    ,[GV.SetMasterRows i | i <- [1,-1]]
    ,[GV.SetMasterCols i | i <- [1,-1]]
    ,[GV.SetMasterFraction r|r<-[]] ]

deriving instance Data Focus
deriving instance Show Focus
deriving instance Read Focus
