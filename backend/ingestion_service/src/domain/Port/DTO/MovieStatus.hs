module Port.DTO.MovieStatus (
  MovieStatus (..),
) where

import Data.Aeson (FromJSON (..), ToJSON (..))
import qualified Data.Aeson as Aeson
import qualified Data.Text as Text
import Overture.Deriving (PrettyJSON (..))

data MovieStatus
  = Rumored
  | Planned
  | InProduction
  | PostProduction
  | Released
  | Canceled
  deriving stock (Eq, Enum)
  deriving (Show) via PrettyJSON MovieStatus

instance FromJSON MovieStatus where
  parseJSON = \case
    Aeson.String "Rumored" -> return Rumored
    Aeson.String "Planned" -> return Planned
    Aeson.String "In Production" -> return InProduction
    Aeson.String "Post Production" -> return PostProduction
    Aeson.String "Released" -> return Released
    Aeson.String "Canceled" -> return Canceled
    Aeson.String unknownStatus ->
      fail ("Unknown movie status: " <> Text.unpack unknownStatus)
    _ -> fail "No string given for movie status"

instance ToJSON MovieStatus where
  toJSON = \case
    Rumored -> Aeson.String "Rumored"
    Planned -> Aeson.String "Planned"
    InProduction -> Aeson.String "In Production"
    PostProduction -> Aeson.String "Post Production"
    Released -> Aeson.String "Released"
    Canceled -> Aeson.String "Canceled"