{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module Port.DTO.ProductionCountry (
  ProductionCountry (..),
) where

import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)
import Deriving.Aeson (CustomJSON (..), FieldLabelModifier, Rename)
import GHC.Generics (Generic)
import Optics (makeFieldLabelsNoPrefix)
import Overture.Deriving (PrettyJSON (..))

data ProductionCountry = ProductionCountry
  { iso31661 :: Text
  , name :: Text
  }
  deriving stock (Eq, Generic)
  deriving (Show) via PrettyJSON ProductionCountry
  deriving
    (FromJSON, ToJSON)
    via CustomJSON
          '[FieldLabelModifier '[Rename "iso31661" "iso_3166_1"]]
          ProductionCountry

makeFieldLabelsNoPrefix ''ProductionCountry