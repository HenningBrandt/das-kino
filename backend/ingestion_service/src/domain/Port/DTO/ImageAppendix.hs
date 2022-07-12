{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module Port.DTO.ImageAppendix (
  ImageAppendix (..),
) where

import Data.Aeson (FromJSON, ToJSON)
import Deriving.Aeson (CustomJSON (..))
import Deriving.Aeson.Stock (Snake)
import GHC.Generics (Generic)
import Optics (makeFieldLabelsNoPrefix)
import Overture.Deriving (PrettyJSON (..))
import Port.DTO.Image (Image)

data ImageAppendix = ImageAppendix
  { backdrops :: [Image]
  , logos :: [Image]
  , posters :: [Image]
  }
  deriving stock (Eq, Generic)
  deriving (Show) via PrettyJSON ImageAppendix
  deriving (FromJSON, ToJSON) via Snake ImageAppendix

makeFieldLabelsNoPrefix ''ImageAppendix