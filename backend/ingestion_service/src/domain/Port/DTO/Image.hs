{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module Port.DTO.Image (
  Image (..),
) where

import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)
import Deriving.Aeson (
  CamelToSnake,
  CustomJSON (..),
  FieldLabelModifier,
  Rename,
 )
import GHC.Generics (Generic)
import Optics (makeFieldLabelsNoPrefix)
import Overture.Deriving (PrettyJSON (..))

data Image = Image
  { aspectRatio :: Float
  , filePath :: Text
  , height :: Int
  , iso6391 :: Maybe Text
  , voteAverage :: Float
  , voteCount :: Int
  , width :: Int
  }
  deriving stock (Eq, Generic)
  deriving (Show) via PrettyJSON Image
  deriving
    (FromJSON, ToJSON)
    via CustomJSON
          '[ FieldLabelModifier
              '[ Rename "iso6391" "iso_639_1"
               , CamelToSnake
               ]
           ]
          Image

makeFieldLabelsNoPrefix ''Image