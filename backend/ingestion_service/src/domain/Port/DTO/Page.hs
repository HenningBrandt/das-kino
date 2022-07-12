{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module Port.DTO.Page (
  Page (..),
) where

import Data.Aeson (FromJSON, ToJSON)
import Deriving.Aeson (CustomJSON (..))
import Deriving.Aeson.Stock (Snake)
import GHC.Generics (Generic)
import Optics (makeFieldLabelsNoPrefix)
import Overture.Deriving (PrettyJSON (..))

data Page a = Page
  { page :: Int
  , totalResults :: Int
  , totalPages :: Int
  , results :: [a]
  }
  deriving stock (Eq, Generic)
  deriving (Show) via PrettyJSON (Page a)
  deriving (FromJSON, ToJSON) via Snake (Page a)

makeFieldLabelsNoPrefix ''Page