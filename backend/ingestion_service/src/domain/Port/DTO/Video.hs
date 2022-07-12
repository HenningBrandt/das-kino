{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module Port.DTO.Video (
  Video (..),
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

data Video = Video
  { iso6391 :: Text
  , iso31661 :: Text
  , name :: Text
  , key :: Text
  , site :: Text
  , size :: Int
  , type' :: Text
  , official :: Bool
  , publishedAt :: Text -- TODO: Use date type
  , id :: Text
  }
  deriving stock (Eq, Generic)
  deriving (Show) via PrettyJSON Video
  deriving
    (FromJSON, ToJSON)
    via CustomJSON
          '[ FieldLabelModifier
              '[ Rename "iso6391" "iso_639_1"
               , Rename "iso31661" "iso_3166_1"
               , Rename "type'" "type"
               , CamelToSnake
               ]
           ]
          Video

makeFieldLabelsNoPrefix ''Video