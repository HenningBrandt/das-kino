{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module Port.DTO.Configuration (
  Configuration (..),
  ImageConfiguration (..),
) where

import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)
import Deriving.Aeson (CustomJSON (..))
import Deriving.Aeson.Stock (Snake)
import GHC.Generics (Generic)
import Optics (makeFieldLabelsNoPrefix)
import Overture.Deriving (PrettyJSON (..))

data Configuration = Configuration
  { images :: ImageConfiguration
  , changeKeys :: [Text]
  }
  deriving stock (Eq, Generic)
  deriving (Show) via PrettyJSON Configuration
  deriving (FromJSON, ToJSON) via Snake Configuration

data ImageConfiguration = ImageConfiguration
  { baseUrl :: Text
  , secureBaseUrl :: Text
  , backdropSizes :: [Text]
  , logoSizes :: [Text]
  , posterSizes :: [Text]
  , profileSizes :: [Text]
  , stillSizes :: [Text]
  }
  deriving stock (Eq, Generic)
  deriving (Show) via PrettyJSON ImageConfiguration
  deriving (FromJSON, ToJSON) via Snake ImageConfiguration

makeFieldLabelsNoPrefix ''Configuration
makeFieldLabelsNoPrefix ''ImageConfiguration