{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module XMonad.Layout.Hinted where

import           XMonad

import           XMonad.Layout.LayoutHints    (layoutHints, layoutHintsToCenter, layoutHintsWithPlacement)
import           XMonad.Layout.LayoutModifier (ModifiedLayout(..))
import           XMonad.Layout.MultiToggle    (Transformer(..))
import           XMonad.Layout.Renamed        (Rename(PrependWords), renamed)

------- instances for MultiToggle ---------
-- TODO: upstream to xmonad-contrib:X.L.LayoutHints

data LayoutHintsTransformers = HINTS -- ^ 'layoutHints'
           | HINTSCENTER -- ^ 'layoutHintsToCenter'
           | HINTSPLACEMENT (Double, Double) -- ^ 'layoutHintsWithPlacement'
           deriving (Eq, Show, Read)

instance Transformer LayoutHintsTransformers Window where
  transform HINTS              x k = k (renamed [PrependWords "Hinted"] $ layoutHints x) (\(ModifiedLayout _ (ModifiedLayout _ l)) -> l)
  transform HINTSCENTER        x k = k (renamed [PrependWords "HintedC"] $ layoutHintsToCenter x) (\(ModifiedLayout _ (ModifiedLayout _ l)) -> l)
  transform (HINTSPLACEMENT a) x k = k (renamed [PrependWords "Hinted'"] $ layoutHintsWithPlacement a x) (\(ModifiedLayout _ (ModifiedLayout _ l)) -> l)
