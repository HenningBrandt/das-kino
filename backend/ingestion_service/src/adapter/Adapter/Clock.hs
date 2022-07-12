module Adapter.Clock (
  adaptClock,
) where

import Control.Exception (bracket)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TextEncoding
import qualified Data.Time as Time
import qualified Data.Time.Zones.DB as TZonesDB
import Polysemy (Embed, Member, Sem, embed, interpret)
import Port.Clock (Clock)
import qualified Port.Clock as Clock
import System.Environment (lookupEnv, setEnv, unsetEnv)

adaptClock :: Member (Embed IO) r => Sem (Clock : r) a -> Sem r a
adaptClock = interpret $ \case
  Clock.GetCurrentTime -> embed $ Time.getCurrentTime
  Clock.GetTimeZone timeZoneLabel -> embed $ do
    let zoneName = Text.unpack . TextEncoding.decodeUtf8 . TZonesDB.toTZName $ timeZoneLabel
    tz <- lookupEnv "TZ"
    bracket
      (setEnv "TZ" zoneName)
      ( \_ -> case tz of
          Nothing -> unsetEnv "TZ"
          Just value -> setEnv "TZ" value
      )
      (\_ -> Time.getCurrentTimeZone)
