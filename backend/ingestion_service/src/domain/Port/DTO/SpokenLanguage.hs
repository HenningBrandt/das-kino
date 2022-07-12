{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module Port.DTO.SpokenLanguage (
  SpokenLanguage (..),
) where

import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)
import Deriving.Aeson (CustomJSON (..), FieldLabelModifier, Rename)
import GHC.Generics (Generic)
import Optics (makeFieldLabelsNoPrefix)
import Overture.Deriving (PrettyJSON (..))

data SpokenLanguage = SpokenLanguage
  { iso6391 :: Text
  , name :: Text
  }
  deriving stock (Eq, Generic)
  deriving (Show) via PrettyJSON SpokenLanguage
  deriving
    (FromJSON, ToJSON)
    via CustomJSON
          '[FieldLabelModifier '[Rename "iso6391" "iso_639_1"]]
          SpokenLanguage

makeFieldLabelsNoPrefix ''SpokenLanguage