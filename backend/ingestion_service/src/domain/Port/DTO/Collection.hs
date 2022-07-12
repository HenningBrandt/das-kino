{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module Port.DTO.Collection (
  Collection (..),
) where

import Data.Aeson (FromJSON)
import Data.Text (Text)
import Deriving.Aeson (CustomJSON (..))
import Deriving.Aeson.Stock (Snake, ToJSON)
import GHC.Generics (Generic)
import Optics (makeFieldLabelsNoPrefix)
import Overture.Deriving (PrettyJSON (..))

data Collection = Collection
  { id :: Int
  , name :: Text
  , posterPath :: Maybe Text
  , backdropPath :: Maybe Text
  }
  deriving stock (Eq, Generic)
  deriving (Show) via PrettyJSON Collection
  deriving (FromJSON, ToJSON) via Snake Collection

makeFieldLabelsNoPrefix ''Collection