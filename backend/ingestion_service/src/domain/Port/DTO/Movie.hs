{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module Port.DTO.Movie (
  Movie (..),
) where

import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)
import Data.Time (Day)
import Deriving.Aeson (CustomJSON (..))
import Deriving.Aeson.Stock (Snake)
import GHC.Generics (Generic)
import Optics (makeFieldLabelsNoPrefix)
import Overture.Deriving (PrettyJSON (..))

data Movie = Movie
  { posterPath :: Maybe Text
  , adult :: Bool
  , overview :: Text
  , releaseDate :: Day
  , genreIds :: [Int]
  , id :: Int
  , originalTitle :: Text
  , originalLanguage :: Text
  , title :: Text
  , backdropPath :: Maybe Text
  , popularity :: Float
  , voteCount :: Int
  , video :: Bool
  , voteAverage :: Float
  }
  deriving stock (Eq, Generic)
  deriving (Show) via PrettyJSON Movie
  deriving (FromJSON, ToJSON) via Snake Movie

makeFieldLabelsNoPrefix ''Movie