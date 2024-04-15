{-# LANGUAGE OverloadedStrings #-}
------------------------------------------------------------------------------
-- |
-- Module      : DesktopNotifications.Markup
-- Description : Desktop notification markup
-- Copyright   : (c) Samuli Thomasson, 2024
-- License     : WTFPL
--
-- Maintainer  : Samuli Thomasson <samuli.thomasson@paivola.fi>
-- Stability   : unstable
-- Portability : unportable
--
------------------------------------------------------------------------------
module DesktopNotifications.Markup where

import           Data.String    (IsString(..))
import           Data.Text      (Text)
import qualified Data.Text      as T
import qualified Data.Text.Lazy as TL
import           DBus           (IsVariant(..))

-- $bodyMarkup
--
-- Fancier servers support full HTML with CSS in the notification body.
-- Lighter ones may only support plain text, though many support the minimum
-- tags from the spec:
--
-- bold @<b></b>@, italics @<i></i>@, underline @<u></u>@,
-- link @<a href="…"></a>@, image @<img src="…" alt="…"/>@.
--
-- If the "body-markup" capability is /not/ present, the server will be
-- displaying the body text as-is. If the capability is present, the server
-- will display or at least strip away markup tags.
--
-- Finally some are somewhere in the middle with extended markup support to
-- that of the minimum. Servers using the Pango library for rendering are
-- likely to support its text markup format.
--
-- https://developer.gnome.org/pango/stable/PangoMarkupFormat.html
--
-- XML '&' escape codes may or may not be understood. Pango markup and heavier
-- HTML-based ones do understand them.

data Markup
  = MRaw Text
  | MLit Text
  | MTag Text [(Text,Text)] (Maybe Markup)
  | MAppend Markup Markup
  | MEmpty
  deriving (Eq, Show, Read)

instance Semigroup Markup where
  (<>) = MAppend

instance Monoid Markup where
  mempty = MEmpty

instance IsString Markup where
  fromString s = MRaw (T.pack s)

instance IsVariant Markup where
  toVariant   = toVariant . markupToText
  fromVariant = fmap MRaw . fromVariant

markupToText :: Markup -> TL.Text
markupToText = go where
  go (MRaw text)             = TL.fromStrict text
  go (MLit text)             = TL.concatMap entities (TL.fromStrict text)
  go (MTag tag attrs minner) = "<" <> TL.fromStrict tag <> toAttrs attrs <> maybe "/>" (\inner -> ">" <> go inner <> "</" <> TL.fromStrict tag <> ">") minner
  go (MAppend ma mb)         = go ma <> go mb
  go MEmpty                  = mempty

  toAttrs xs = TL.concat [ " " <> TL.fromStrict a <> "=\"" <> TL.concatMap entities (TL.fromStrict v) <> "\"" |(a,v) <- xs]

  entities c = case c of
                 '>'  -> "&gt;"
                 '<'  -> "&lt;"
                 '&'  -> "&amp;"
                 '\"' -> "&quot;"
                 _    -> TL.pack [c]

raw :: String -> Markup
raw = MRaw . T.pack

str :: String -> Markup
str = MLit . T.pack

bold, italic, underline, styleTT :: Markup -> Markup
bold inner      = MTag "b"  [] (Just inner)
italic inner    = MTag "i"  [] (Just inner)
underline inner = MTag "u"  [] (Just inner)
styleTT inner   = MTag "tt" [] (Just inner)

link :: Text -> Markup -> Markup
link url inner = MTag "a" [("href",url)] (Just inner)

-- | @img url description@
img :: Text -> Text -> Markup
img src alt = MTag "img" [("src",src),("alt",alt)] Nothing
