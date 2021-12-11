
------------------------------------------------------------------------------
-- |
-- Module      : StatusBar.XMobar
-- Description : Short description
-- Copyright   : (c) Samuli Thomasson, 2021
-- License     : WTFPL
--
-- Maintainer  : Samuli Thomasson <samuli.thomasson@paivola.fi>
-- Stability   : unstable
-- Portability : unportable
--
------------------------------------------------------------------------------

module StatusBar.XMobar where

import           XMonad
import           XMonad.Hooks.DynamicLog

import qualified Xmobar                  as XB

import qualified DBus
import qualified DBus.Client
import           Data.String             (IsString(..))
import qualified System.Directory
import qualified System.Random           as Random
import           Text.Printf             (printf)

import           MyTheme

-- * Xmobar Config Builder

data ConfigB
  -- | Add simple String literal to the resulting template.
  = LitB String
  -- | Concatenate.
  | ConcatB [ConfigB]
  -- | Modify the template of inner ConfigB.
  | ApplyB (String -> String) ConfigB
  -- | Widget: CommandB TEMPLATE_STRING COMMAND
  | CommandB String XB.Runnable
  -- | Widget with unique template string (auto-generated)
  | CommandUniqB (String -> XB.Runnable)
  -- | If-then-else on ConfigB.
  | OnIfB (IO Bool) ConfigB ConfigB
  -- | Catch-all arbitrary AST modify. XXX
  | SetConfigB (XB.Config -> XB.Config)

instance Semigroup ConfigB where
  (ConcatB xs) <> (ConcatB ys) = ConcatB (xs ++ ys)
  (ConcatB xs) <> y            = ConcatB (xs ++ [y])
  x <> (ConcatB ys)            = ConcatB (x : ys)
  x <> y                       = ConcatB [x,y]

instance Monoid ConfigB where
  mempty = ConcatB []

instance IsString ConfigB where
  fromString = LitB

fromConfigB :: ConfigB -> IO XB.Config
fromConfigB cb = go' cb base
  where
    go' x cfg@XB.Config{XB.commands = cmds', XB.template = tmpl'} =
      case x of
        SetConfigB f    -> pure $ f cfg
        CommandB t r    -> pure $ cfg { XB.commands = r : cmds', XB.template = tmpl' ++ t }
        CommandUniqB m  -> do
          nm <- newName
          go' (CommandB ("%" ++ nm ++ "%") (m nm)) cfg
        LitB s          -> pure $ cfg { XB.template = tmpl' ++ s }
        ApplyB f x'     -> do
          cfg' <- go' x' cfg { XB.template = "" }
          pure $ cfg' { XB.template = tmpl' ++ f (XB.template cfg') }
        ConcatB []      -> pure cfg
        ConcatB (x':xs) -> do
          cfg' <- go' x' cfg
          go' (ConcatB xs) cfg'
        OnIfB mbool x' y' -> do
          bool <- mbool
          if bool then go' x' cfg else go' y' cfg

    base = XB.defaultConfig { XB.commands = [], XB.template = "" }
    newName = take 8 . Random.randomRs ('a','z') <$> Random.newStdGen

sepByB :: ConfigB -> [ConfigB] -> ConfigB
sepByB sep = go mempty where
  go rs (OnIfB m x (ConcatB []):ys) = OnIfB m (rs <> x <> go sep ys) (go rs ys)
  go rs (x:ys)                      = rs <> x <> go sep ys
  go _  []                          = mempty

modifyConfigB :: (XB.Config -> XB.Config) -> ConfigB
modifyConfigB = SetConfigB

setFontsB :: [MyTheme.Font] -> ConfigB
setFontsB []           = mempty
setFontsB (main:extra) = modifyConfigB $ \cfg -> cfg
  { XB.font            = show main
  , XB.additionalFonts = show <$> extra
  }

litB :: String -> ConfigB
litB = LitB

wrapB :: (String -> String) -> ConfigB -> ConfigB
wrapB = ApplyB

fgB :: String -> ConfigB -> ConfigB
fgB c = wrapB (fg c)

boxB :: BoxOptions -> ConfigB -> ConfigB
boxB = wrapB . box'

fg :: String -> String -> String
fg c = xmobarColor c ""

xmobarFont :: Int -> String -> String
xmobarFont i = wrap (printf "<fn=%i>" i) "</fn>"

-- * Commands

kbdB :: ConfigB
kbdB = CommandB "%kbd%" $ XB.Run $ XB.Kbd [("fi", "QWR"), ("dvp(dvp)","DVP")]

locksB :: ConfigB
locksB = CommandB "%locks%" (XB.Run XB.Locks)

dateB :: String -> XB.Rate -> ConfigB
dateB fmt r = CommandB "%date%" (XB.Run $ XB.Date fmt "date" r)

-- | dateZoneB format locale zone rate. locale and zone may be @""@
dateZoneB :: String -> String -> String -> XB.Rate -> ConfigB
dateZoneB fmt locale zone r = CommandUniqB $ \nm -> XB.Run $ XB.DateZone fmt locale zone nm r

comB :: String -> [String] -> XB.Rate -> ConfigB
comB cmd args r = CommandUniqB $ \nm -> XB.Run $ XB.Com cmd args nm r

pipeReaderB :: String -> FilePath -> ConfigB
pipeReaderB defText infile = CommandUniqB $ \nm -> XB.Run $ XB.PipeReader (printf "%s:%s" defText infile) nm

bufferedPipeReaderB :: [(Int, Bool, FilePath)] -> ConfigB
bufferedPipeReaderB xs = CommandUniqB $ \nm -> XB.Run $ XB.BufferedPipeReader nm xs

-- | %<mixer>:<element>%
volumeB :: String -> String -> MonArgs -> XB.Rate -> ConfigB
volumeB mixer element args r = CommandB ("%" ++ mixer ++ ":" ++ element ++ "%") (XB.Run $ XB.Volume mixer element (renderMonArgs args) r)

-- | alsaB "default" "Master"
alsaB :: String -> String -> MonArgs -> ConfigB
alsaB mixer element args = CommandB ("%" ++ printf "alsa:%s:%s" mixer element ++ "%") $ XB.Run $ XB.Alsa mixer element (renderMonArgs args)

-- | @weatherB <skyCnoditions> <station> <args> <rate>@
--
-- http://tgftp.nws.noaa.gov
-- https://en.wikipedia.org/wiki/ICAO_airport_code
--
-- LOWG: Graz-Thalerhof-Flughafen, Austria
-- LOWW: Wien / Schwechat-Flughafen, Austria
weatherB :: [(String,String)] -> String -> MonArgs -> XB.Rate -> ConfigB
weatherB skyConditions station args r = CommandB ("%" ++ station ++ "%") $ XB.Run $ XB.WeatherX station skyConditions (renderMonArgs args) r

dynnetworkB :: MonArgs -> XB.Rate -> ConfigB
dynnetworkB args r = CommandB "%dynnetwork%" $ XB.Run $ XB.DynNetwork (renderMonArgs args) r

multiCpuB :: MonArgs -> XB.Rate -> ConfigB
multiCpuB args r = CommandB "%multicpu%" $ XB.Run $ XB.MultiCpu (renderMonArgs args) r

memoryB :: MonArgs -> XB.Rate -> ConfigB
memoryB args r = CommandB "%memory%" $ XB.Run $ XB.Memory (renderMonArgs args) r

topProcB :: MonArgs -> XB.Rate -> ConfigB
topProcB args r = CommandB "%top%" $ XB.Run $ XB.TopProc (renderMonArgs args) r

topMemB :: MonArgs -> XB.Rate -> ConfigB
topMemB args r = CommandB "%topmem%" $ XB.Run $ XB.TopMem (renderMonArgs args) r

mpdB :: MonArgs -> XB.Rate -> ConfigB
mpdB args r = CommandB "%mpd%" $ XB.Run $ XB.MPD (renderMonArgs args) r

batteryB :: MonArgs -> XB.Rate -> ConfigB
batteryB args r = OnIfB check (batteryB' args r) mempty where
  check = not . null <$> System.Directory.listDirectory "/sys/class/power_supply"

batteryB' :: MonArgs -> XB.Rate -> ConfigB
batteryB' args r = CommandB "%battery%" $ XB.Run $ XB.Battery (renderMonArgs args) r

-- @wireless "wlp2s0"@ TODO
-- [ "-t", "<essid>", "-p", "2" ]
wirelessB :: XB.Interface -> MonArgs -> XB.Rate -> ConfigB
wirelessB iface args r = CommandB ("%" ++ iface ++ "wi%") $ XB.Run $ XB.Wireless iface (renderMonArgs args) r

-- * Monitor Arguments

data MonArgs = MonArgs
  { monTemplate                               :: String -- -t (template)
  , monHigh, monLow                           :: Int -- -H -L (thresholds)
  , monHighColor, monLowColor, monNormalColor :: String -- -h -l -n (colors)
  , monSuffix                                 :: Bool -- -S True (default False)
  , monPercentagePadding                      :: Int -- -p (default 0)
  , monDecimalDigits                          :: Int -- -d (default 0)
  , monFieldWidthMin, monFieldWidthMax        :: Int -- -m -M (default 0)
  , monFieldEllipsis, monTotalEllipsis        :: String -- -e -E (default "")
  , monFieldWidthFixed, monTotalWidthMax      :: Int -- -w -T (default 0)
  , monPaddingChars                           :: String -- -c (default " ")
  , monFieldAlign                             :: Char -- -a r|l (default 'r')
  , monBarBg, monBarFg                        :: String -- -b -f (default ":", "#")
  , monBarWidth                               :: Int -- -W (default 10)
  , monNotAvailable                           :: String -- -x (default "N/A")
  , monExtraArgs                              :: [String] -- -- ... (monitor-specific)
  } deriving (Show)

instance Default MonArgs where
  def = MonArgs
    { monTemplate = ""
    , monHigh = 66
    , monLow = 33
    , monHighColor = ""
    , monLowColor = ""
    , monNormalColor = ""
    , monSuffix = False
    , monPercentagePadding = 0
    , monDecimalDigits = 0
    , monFieldWidthMin = 0
    , monFieldWidthMax = 0
    , monFieldWidthFixed = 0
    , monTotalWidthMax = 0
    , monFieldEllipsis = ""
    , monTotalEllipsis = ""
    , monPaddingChars = " "
    , monFieldAlign = 'r'
    , monBarBg = ":"
    , monBarFg = "#"
    , monBarWidth = 10
    , monNotAvailable = "N/A"
    , monExtraArgs = []
    }

renderMonArgs :: MonArgs -> [String]
renderMonArgs args = go 't' monTemplate id
    ++ go 'H' monHigh show ++ go 'L' monLow show
    ++ go 'h' monHighColor id ++ go 'l' monLowColor id ++ go 'n' monNormalColor id
    ++ go 'S' monSuffix (const "True")
    ++ go 'p' monPercentagePadding show ++ go 'd' monDecimalDigits show
    ++ go 'm' monFieldWidthMin show ++ go 'M' monFieldWidthMax show
    ++ go 'e' monFieldEllipsis id ++ go 'E' monTotalEllipsis id
    ++ go 'w' monFieldWidthFixed show ++ go 'T' monTotalWidthMax show
    ++ go 'c' monPaddingChars id
    ++ go 'a' monFieldAlign pure
    ++ go 'b' monBarBg id
    ++ go 'f' monBarFg id
    ++ go 'W' monBarWidth show
    ++ go 'x' monNotAvailable id
    ++ concat ["--":monExtraArgs args | monExtraArgs args /= []]
  where
    go c f g
      | f def /= f args = [['-',c], g (f args)]
      | otherwise       = []

-- * Boxes (<box>)

data BoxOptions = BoxOptions
  { boxType   :: BoxType
  , boxColor  :: String
  , boxWidth  :: Int
  , boxOffset :: (XB.Align, Int)
  , boxMargin :: [Int] -- top,bottom,left,right
  } deriving Eq

data BoxType = BTop | BBottom | BVBoth | BLeft | BRight | BHBoth | BFull
  deriving (Show, Eq, Enum)

instance Default BoxOptions where
  def = BoxOptions BHBoth "" 0 (XB.C, 0) []

-- | Box a string.
box' :: BoxOptions -> String -> String
box' opts = wrap ("<box " ++ renderBoxOptions opts ++ ">") "</box>"

-- | Box a string with given padding.
boxP :: Int -> String -> String
boxP bw =
  box' def{ boxType = BVBoth, boxColor = colBase01, boxMargin = [bw,bw,0,0] } .
  box' def{ boxType = BHBoth, boxColor = colBase01, boxOffset = (XB.C,bw) }

-- | Render box options (internal).
renderBoxOptions :: BoxOptions -> String
renderBoxOptions opts = unwords $
  [ "type=" <> drop 1 (show bt) | bt <- [boxType opts], bt /= BFull ]
  ++ [ "color=" <> bc | bc <- [boxColor opts], bc /= "" ]
  ++ [ "width=" <> show bw | bw <- [boxWidth opts], bw > 0 ]
  ++ [ printf "offset=%s%i" [c] px | (a,px) <- [boxOffset opts], px > 0, let c = case a of {XB.L->'L';XB.R->'R';XB.C->'C'} ]
  ++ [ printf "m%s=%i" [c] px | (c,px) <- zip "tblr" (boxMargin opts), px > 0 ]

-- * Literals

-- | Spaces
enspace, emspace, puncsp, thinsp, hairsp, numsp :: String
enspace = "\x2002"
emspace = "\x2003"
puncsp  = "\x2008"
thinsp  = "\x2009"
hairsp  = "\x200A"
numsp   = "\x2007" -- same width as numerical digit

-- | Dashes
figdash, endash, emdash, swungdash, oendash :: String
figdash   = "\x2012" -- same width as numerical digit
endash    = "\x2013" -- half em dash
emdash    = "\x2014" -- "—"
swungdash = "\x2053" -- "⁓"
oendash   = wrap hairsp hairsp endash -- open en dash

-- * Xmobar D-Bus methods

sendWakeup :: X ()
sendWakeup = safeSendSignal ["Wakeup"]

safeSendSignal :: [String] -> X ()
safeSendSignal s = catchX (io $ sendSignal s) (return ())
  where
    sendSignal = withSession . callSignal
    withSession mc = DBus.Client.connectSession >>= \c -> DBus.Client.callNoReply c mc >> DBus.Client.disconnect c
    callSignal sig = (DBus.methodCall
      (DBus.objectPath_    "/org/Xmobar/Control")
      (DBus.interfaceName_ "org.Xmobar.Control" )
      (DBus.memberName_    "SendSignal"         )
      ) { DBus.methodCallDestination = Just $ DBus.busName_ "org.Xmobar.Control"
        , DBus.methodCallBody        = map DBus.toVariant sig }

