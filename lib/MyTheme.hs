{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}


------------------------------------------------------------------------------
-- |
-- Module      : MyTheme
-- Description : Short description
-- Copyright   : (c) Samuli Thomasson, 2019
-- License     : BSD-3
--
-- Maintainer  : Samuli Thomasson <samuli.thomasson@relexsolutions.com>
-- Stability   : unstable
-- Portability : unportable
--
------------------------------------------------------------------------------

module MyTheme
  ( sepByConcat
  , Font(..)
  , FontSize(..)
  , font
  , font2
  , wqyMicroHei
  , terminessPowerline
  , terminessNerd
  , bfont
  , module XMonad.Config.Solarized
  , gsconfig1
  , xpConfig
  , xpConfigAuto
  , xpConfigNoHist
  ) where

import           XMonad                        hiding (Font, title)
import qualified XMonad                        as X

import qualified XMonad.Actions.GridSelect     as GS
import qualified XMonad.Prompt                 as XP
import           XMonad.Prompt.FuzzyMatch      (fuzzyMatch, fuzzySort)

import qualified Data.Map
import qualified Data.Char                     as Char
import           Data.Foldable
import qualified Data.List
import           Data.Ratio
import           Prelude
import           Text.Printf                   (printf)

import           XMonad.Config.Solarized

-- "Data.List.intersperse" generalized for any "Foldable" of some "Monoid".
sepByConcat :: (Monoid a, Foldable t) => a -> t a -> a
sepByConcat a = mconcat . Data.List.intersperse a . toList

terminus           = def { fontFamily = "xos4 Terminus" }
terminessPowerline = def { fontFamily = "xos4 Terminess Powerline" }
terminessNerd      = def { fontFamily = "TerminessTTF Nerd Font" }
wqyMicroHei size   = def { fontFamily = "WenQuanYi Micro Hei", fontSize = Just (PixelSize size) }
notoSansLight      = def { fontFamily = "Noto Sans Light", fontSize = Just (PointSize 8) }
notoMono           = def { fontFamily = "Noto Sans Mono", fontSize = Just (PointSize 8) }

font, bfont :: Int -> Font
font size = sel { fontSize = Just (PixelSize size) } where
  sel | size <= 14 = terminessPowerline
      | otherwise  = terminessNerd
bfont size = (font size) { fontStyle = Just Bold }

font2 = notoMono

data Font = Font
  { fontFamily :: String
  , fontSize   :: Maybe FontSize
  , fontStyle  :: Maybe FontStyle
  } deriving (Eq)

instance Show Font where
  show Font{..} = "xft:" <> fontFamily <> maybe "" dSize fontSize <> dStyle fontStyle where
    dSize (PixelSize s) = printf ":pixelsize=%i" s
    dSize (PointSize d) = printf ":size=%.2f" d
    dStyle s = case maybe "" show s of
                 x:xs -> ':' : x : [r | c<-xs, r <- [' '|Char.isUpper c]++[c]]
                 []   -> ""

instance X.Default Font where
  def = Font "monospace" def def

data FontSize  = PixelSize Int | PointSize Double
  deriving (Eq)

data FontStyle = Thin | Medium | Regular | Bold | BoldItalic | Light | LightItalic | Semibold | Italic
  deriving (Eq, Show)

gsconfig1 :: GS.HasColorizer a => GS.GSConfig a
gsconfig1 = def
  { GS.gs_cellwidth   = 360
  , GS.gs_cellheight  = 24
  , GS.gs_cellpadding = 5
  , GS.gs_navigate    = GS.navNSearch
  , GS.gs_font        = show font2 -- (font 18)
  , GS.gs_bordercolor = colCyan
  }

xpConfig :: XP.XPConfig
xpConfig = def
  { XP.font                = show (font2) --  18)
  , XP.bgColor             = colBase03
  , XP.fgColor             = colBase1
  , XP.fgHLight            = colMagenta
  , XP.bgHLight            = colBase02
  , XP.borderColor         = colCyan
  , XP.promptBorderWidth   = 1
  , XP.position            = XP.CenteredAt (1%3) (1%2)
  , XP.height              = 35 -- per row
  , XP.maxComplRows        = Just 40
  , XP.historySize         = 512
  , XP.historyFilter       = XP.deleteAllDuplicates
  , XP.promptKeymap        = myPromptKeymap
  , XP.completionKey       = (0, X.xK_Tab)
  , XP.changeModeKey       = X.xK_grave -- ` i.e. <S-#>
  , XP.showCompletionOnTab = False  -- only show list of completions when tab was pressed (False)
  , XP.searchPredicate     = fuzzyMatch
  , XP.defaultPrompter     = ("[xmonad] " ++)
  , XP.sorter              = fuzzySort
  }

myPromptKeymap = XP.defaultXPKeymap <> Data.Map.fromList
  [((X.shiftMask, X.xK_Insert), XP.pasteString)]

xpConfigAuto, xpConfigNoHist :: XP.XPConfig
xpConfigAuto   = xpConfig { XP.autoComplete = Just 500000 }
xpConfigNoHist = xpConfig { XP.historySize = 0 }
