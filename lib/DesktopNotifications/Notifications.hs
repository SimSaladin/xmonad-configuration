{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TypeApplications           #-}


------------------------------------------------------------------------------
-- |
-- Module      : DesktopNotifications.Notifications
-- Description : Notifications
-- Copyright   : (c) Samuli Thomasson, 2024
-- License     : WTFPL
--
-- Maintainer  : Samuli Thomasson <samuli.thomasson@paivola.fi>
-- Stability   : unstable
-- Portability : unportable
--
------------------------------------------------------------------------------
module DesktopNotifications.Notifications where

import           Data.Int    (Int32)
import qualified Data.Map    as M
import           Data.String (IsString(..))
import           Data.Text   (Text)
import           Data.Word   (Word32, Word8)
import           DBus        hiding (Type)
import           Text.Printf (PrintfArg)
import           XMonad      (Default(..))

data Note text = Note
  { notify_app_name       :: !Text                   -- ^ @STRING@
  , notify_replaces_id    :: !NotifyId               -- ^ @UINT32@
  , notify_app_icon       :: !FilePath               -- ^ @STRING@
  , notify_summary        :: !text                   -- ^ @STRING@
  , notify_body           :: !text                   -- ^ @STRING@
  , notify_actions        :: !(M.Map ActionId Text)  -- ^ @ARRAY@ (@[id0, name0, id1, name1, ...]@)
  , notify_hints          :: !(M.Map Text Variant)   -- ^ @DICT@
  , notify_expire_timeout :: !ExpireTimeout          -- ^ @INT32@
  } deriving (Eq, Show)

instance Monoid t => Default (Note t) where
  def = Note mempty def def mempty mempty mempty mempty def

instance IsString (Note Text) where
  fromString s = def { notify_body = fromString s }

instance IsVariant t => Semigroup (Note t) where
  n1 <> n2 = Note
    { notify_app_name = f notify_app_name (/= "")
    , notify_replaces_id = f notify_replaces_id (/= def)
    , notify_app_icon = f notify_app_icon (/= def)
    , notify_summary = f notify_summary ((/= toVariant @Text "") . toVariant)
    , notify_body = f notify_body ((/= toVariant @Text "") . toVariant)
    , notify_actions = notify_actions n1 <> notify_actions n2
    , notify_hints = notify_hints n1 <> notify_hints n2
    , notify_expire_timeout = f notify_expire_timeout (/= def)
    } where
      f field g
        | g (field n2) = field n2
        | otherwise = field n1

newtype NotifyId = NotifyId Word32
  deriving (Eq, Ord, Show, Read, Enum, Num, Real, Bounded, Integral, PrintfArg, Default, IsVariant)

newtype ExpireTimeout = ExpireTimeout Int32
  deriving (Eq, Ord, Show, Read, Enum, Num, Real, Bounded, Integral, PrintfArg, IsVariant)

instance Default ExpireTimeout where
  def = fromIntegral (-1 :: Int)

type ActionId = Text

newtype ImageData = ImageData { fromImageData :: Variant }
  deriving (Eq, Show, IsVariant)

type Notify = Note Text

newtype Urgency = Urgency { fromUrgency :: Word8 }
  deriving (Eq, Ord, Show, Read, IsVariant)

toVariants :: IsVariant t => Note t -> [Variant]
toVariants Note{..} =
  [ toVariant notify_app_name
  , toVariant notify_replaces_id
  , toVariant notify_app_icon
  , toVariant notify_summary
  , toVariant notify_body
  , toVariant (concat [[k,v]|(k,v)<-M.toAscList notify_actions])
  , toVariant notify_hints
  , toVariant notify_expire_timeout
  ]

summary :: t -> Note t -> Note t
summary s nt = nt { notify_summary = s }

summaryDef :: (Eq t, Monoid t) => t -> Note t -> Note t
summaryDef s nt
  | notify_summary nt == mempty = nt { notify_summary = s }
  | otherwise = nt

body :: t -> Note t -> Note t
body b nt = nt { notify_body = b }

appName :: Text -> Note t -> Note t
appName name nt = nt { notify_app_name = name }

appNameDef :: Text -> Note t -> Note t
appNameDef name nt
  | notify_app_name nt == mempty = nt { notify_app_name = name }
  | otherwise = nt

replaces :: NotifyId -> Note t -> Note t
replaces nid nt = nt { notify_replaces_id = nid }

-- ** Hints

-- | General hints. @myHint = hintF "hint-name" hintValue@
hintF :: IsVariant hint => Text -> hint -> Note t -> Note t
hintF k v nt = nt { notify_hints = M.alter (\_ -> Just $ toVariant v) k $ notify_hints nt }

-- | The server might display notifications of different categories
-- differently. Notification categories are semi-standardized, see the
-- reference.
--
-- https://developer.gnome.org/notification-spec/#categories
--
-- Hint @category@
category :: String -> Note t -> Note t
category = hintF "category"

-- | This field can be used to specify the calling application's desktop
-- entry, which might be used to for example find out an app icon to display
-- in the notification. (Specify without the @.desktop@ suffix.)
--
-- Hint @desktop-entry@
desktopEntry :: String -> Note t -> Note t
desktopEntry = hintF "desktop-entry"

-- | Expiration timeout (milliseconds)
--
--   * -1: use server settings (default)
--   * 0: never expire
expires :: Integral t => t -> Note t -> Note t
expires t nt = nt { notify_expire_timeout = min (-1) (fromIntegral t) }

-- | Include an action is the notification. If the server supports it, it can
-- display the actions for user to activate. Activation is reported back to
-- the client as signals.
--
-- The default action should have a key (@ActionId@) named @"default"@:
-- @
-- action "default" "Default action"
-- @
action :: ActionId -> Text -> Note t -> Note t
action act name nt = nt { notify_actions = M.insert act name (notify_actions nt) }

-- | The notification should persist until its explicitly dismissed or closed.
--
-- Hint @resident@
resident :: Note t -> Note t
resident = hintF "resident" True

-- | Over-ride server persistence capability, should that exist.
--
-- Hint @transient@
transient :: Note t -> Note t
transient = hintF "transient" True

-- | Set explicit position for the notification.
--
-- Hints @x@ and @y@
position :: Int32 -> Int32 -> Note t -> Note t
position x y = hintF "x" x . hintF "y" y

-- | Urgency Level
--
--   * 0 Low
--   * 1 Normal (default if not set)
--   * 2 Critical
urgency :: Urgency -> Note t -> Note t
urgency = hintF "urgency"

-- | Critical notifications do not usually expire automatically.
low, normal, critical :: Urgency
low = Urgency 0
normal = Urgency 1
critical = Urgency 2

-- ** Icons & images

-- | The server can attempt to use action keys in the notification as icon names.
--
-- Hint @action-icons@
actionIcons :: Note t -> Note t
actionIcons = hintF "action-icons" True

appIcon :: String -> Note t -> Note t
appIcon icon nt = nt { notify_app_icon = icon }

appIconDef :: String -> Note t -> Note t
appIconDef icon nt
  | notify_app_icon nt == mempty = nt { notify_app_icon = icon }
  | otherwise = nt

-- | Display image given by path to local file.
--
-- @
-- imagePath "/usr/share/icons/Adwaita/32x32/emotes/face-plain-symbolic.symbolic.png"
-- @
--
-- https://developer.gnome.org/notification-spec/#icons-and-images
--
-- Hint @image-path@.
imagePath :: FilePath -> Note t -> Note t
imagePath path = hintF "image-path" ("file://" ++ path)

-- | Give an image directly in the notification itself. If both this and an
-- icon is specified then behavior is server-specific.
--
-- This is probably only useful if you want to display modified (or generated)
-- images, or images not on the local filesystem. Most notification servers
-- can display images from local files (see "imagePath").
--
-- As defined by the spec, data has to be a raw image (formatted per
-- "imageDataSignature"). To use this hint with other image formats, one
-- could use a library such as https://hackage.haskell.org/package/JuicyPixels
-- or an external tool like ImageMagick to convert any image into the correct
-- format (see "ImageData").
--
-- If the notification server supports it (capability @body-images@), you
-- could opt to use the @<img src="..." alt="..." />@ body markup instead.
imageData :: ImageData -> Note t -> Note t
imageData = hintF "image-data"

-- | The image must be given in the data structure of dbus signature @(iiibiiay)@, e.g. RAW.
-- Respectively width, height, rowstride, has alpha, bits per sample, channels, image data.
--
-- @
-- > signatureTypes imageDataSignature
-- [(Int32, Int32, Int32, Bool, Int32, Int32, [Word8])]
-- @
imageDataSignature :: Signature
imageDataSignature = "(iiibiiay)"

-- ** Sound

-- | Play sound given by path to local file.
--
-- Hint @sound-file@
soundFile :: String -> Note t -> Note t
soundFile = hintF "sound-file"

-- | Play sound given by themeable sound name. See reference for names.
--
-- http://0pointer.de/public/sound-naming-spec.html
--
-- Hint @sound-name@
soundName :: String -> Note t -> Note t
soundName = hintF "sound-name"

-- | Hint @suppress-sound@
suppressSound :: Note t -> Note t
suppressSound = hintF "suppress-sound" True
