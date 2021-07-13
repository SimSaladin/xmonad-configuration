{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances     #-}

module XMonad.Layout.Hinted where

import           XMonad

import           XMonad.Layout.LayoutHints    ({-layoutHintsToCenter,-} layoutHintsWithPlacement)
import           XMonad.Layout.LayoutModifier (ModifiedLayout(..))
import           XMonad.Layout.MultiToggle    (Transformer(..))
import           XMonad.Layout.Renamed        (Rename(PrependWords), renamed)

data HINT = HINT deriving (Eq, Show, Read)

instance Transformer HINT Window where
  transform HINT x k = k
    --(renamed [PrependWords "Hint"] $ layoutHintsToCenter x)
    (renamed [PrependWords "Hint"] $ layoutHintsWithPlacement (0.5,0.5) x)
    (\(ModifiedLayout _ (ModifiedLayout _ l)) -> l)
