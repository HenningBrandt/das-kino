{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module Port.DTO.ProductionCompany (
  ProductionCompany (..),
) where

import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)
import Deriving.Aeson (CustomJSON (..))
import Deriving.Aeson.Stock (Snake)
import GHC.Generics (Generic)
import Optics (makeFieldLabelsNoPrefix)
import Overture.Deriving (PrettyJSON (..))

data ProductionCompany = ProductionCompany
  { name :: Text
  , id :: Int
  , logoPath :: Maybe Text
  , originCountry :: Text
  }
  deriving stock (Eq, Generic)
  deriving (Show) via PrettyJSON ProductionCompany
  deriving (FromJSON, ToJSON) via Snake ProductionCompany

makeFieldLabelsNoPrefix ''ProductionCompany