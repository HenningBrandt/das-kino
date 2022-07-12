{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module Port.DTO.Genre (
  Genre (..),
) where

import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)
import Deriving.Aeson (CustomJSON (..))
import Deriving.Aeson.Stock (Snake)
import GHC.Generics (Generic)
import Optics (makeFieldLabelsNoPrefix)
import Overture.Deriving (PrettyJSON (..))

data Genre = Genre
  { id :: Int
  , name :: Text
  }
  deriving stock (Eq, Generic)
  deriving (Show) via PrettyJSON Genre
  deriving (FromJSON, ToJSON) via Snake Genre

makeFieldLabelsNoPrefix ''Genre