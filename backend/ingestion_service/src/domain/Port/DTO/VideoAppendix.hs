{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module Port.DTO.VideoAppendix (
  VideoAppendix (..),
) where

import Data.Aeson (FromJSON, ToJSON)
import Deriving.Aeson (CustomJSON (..))
import Deriving.Aeson.Stock (Snake)
import GHC.Generics (Generic)
import Optics (makeFieldLabelsNoPrefix)
import Overture.Deriving (PrettyJSON (..))
import Port.DTO.Video (Video)

newtype VideoAppendix = VideoAppendix
  { results :: [Video]
  }
  deriving stock (Eq, Generic)
  deriving (Show) via PrettyJSON VideoAppendix
  deriving (FromJSON, ToJSON) via Snake VideoAppendix

makeFieldLabelsNoPrefix ''VideoAppendix