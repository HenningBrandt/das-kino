{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module Domain.Movie (
  Movie (..),
  LocalTimeMovie,
  UTCMovie,
  ShowTime (..),
  LocalTimeShowTime,
  UTCShowTime,
  BookingLink (..),
  Date (..),
  LocalTimeDate,
  Format (..),
  isSame,
  mergeShowTimes,
  convertToUTC,
) where

import Data.Aeson (FromJSON (parseJSON), ToJSON (toJSON))
import qualified Data.Aeson as Aeson
import Data.Function (on)
import qualified Data.List.GroupBy as GroupBy
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import Data.Time (Day, LocalTime (LocalTime), TimeOfDay, TimeZone, UTCTime)
import qualified Data.Time as Time
import Deriving.Aeson (CustomJSON (..), SumUntaggedValue)
import Deriving.Aeson.Stock (Snake, Vanilla)
import GHC.Generics (Generic)
import Optics
import Overture.Deriving (PrettyJSON (..))
import Text.URI (URI)
import qualified Text.URI as URI

data Movie time = Movie
  { name :: Text
  , showTimes :: [ShowTime time]
  }
  deriving stock (Eq, Generic)
  deriving (Show) via PrettyJSON (Movie time)
  deriving (FromJSON, ToJSON) via Snake (Movie time)

type LocalTimeMovie = Movie LocalTime
type UTCMovie = Movie UTCTime

data ShowTime time = ShowTime
  { date :: Date time
  , format :: Format
  , bookingLink :: Maybe BookingLink
  }
  deriving stock (Eq, Generic)
  deriving (Show) via PrettyJSON (ShowTime time)
  deriving (FromJSON, ToJSON) via Snake (ShowTime time)

type LocalTimeShowTime = ShowTime LocalTime
type UTCShowTime = ShowTime UTCTime

newtype BookingLink = BookingLink
  {uri :: URI}
  deriving newtype (Eq, Show)

instance ToJSON BookingLink where
  toJSON = Aeson.String . URI.render . (^. #uri)

instance FromJSON BookingLink where
  parseJSON = \case
    Aeson.String txt -> case URI.mkURI txt of
      Left error -> fail (show error)
      Right uri -> return (BookingLink uri)
    _ -> fail "Expected a string"

data Date time = Today TimeOfDay | Date time
  deriving stock (Eq, Generic)
  deriving (Show) via PrettyJSON (Date time)
  deriving
    (FromJSON, ToJSON)
    via CustomJSON '[SumUntaggedValue] (Date time)

type LocalTimeDate = Date LocalTime

data Format = Format2D | Format3D
  deriving stock (Eq, Enum, Generic)
  deriving (Show) via PrettyJSON Format
  deriving (FromJSON, ToJSON) via Vanilla Format

makeFieldLabelsNoPrefix ''Movie
makeFieldLabelsNoPrefix ''ShowTime
makeFieldLabelsNoPrefix ''BookingLink

isSame :: Movie d -> Movie d -> Bool
isSame = (==) `on` (^. #name)

mergeShowTimes :: [Movie d] -> [Movie d] -> [Movie d]
mergeShowTimes ms0 ms1 = mapMaybe mergeGroups groups
 where
  groups = GroupBy.groupBy isSame (ms0 ++ ms1)
  mergeGroups = \case
    [] -> Nothing
    (m : ms) -> Just (foldr merge m ms)
  merge m0 m1 = Movie (m0 ^. #name) (m0 ^. #showTimes ++ m1 ^. #showTimes)

convertToUTC :: TimeZone -> Day -> [LocalTimeMovie] -> [UTCMovie]
convertToUTC tz today = over (each % #showTimes % each % #date) convert
 where
  convert = \case
    Today timeOfDay -> Date $ Time.localTimeToUTC tz (LocalTime today timeOfDay)
    Date localTime -> Date $ Time.localTimeToUTC tz localTime