{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module Port.DTO.MovieDetail (
  MovieDetail (..),
) where

import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)
import Data.Time (Day)
import Deriving.Aeson (CustomJSON (..))
import Deriving.Aeson.Stock (Snake)
import GHC.Generics (Generic)
import Optics (makeFieldLabelsNoPrefix)
import Overture.Deriving (PrettyJSON (..))
import Port.DTO.Collection (Collection)
import Port.DTO.Genre (Genre)
import Port.DTO.ImageAppendix (ImageAppendix)
import Port.DTO.MovieStatus (MovieStatus)
import Port.DTO.ProductionCompany (ProductionCompany)
import Port.DTO.ProductionCountry (ProductionCountry)
import Port.DTO.SpokenLanguage (SpokenLanguage)
import Port.DTO.VideoAppendix (VideoAppendix)

data MovieDetail = MovieDetail
  { adult :: Bool
  , backdropPath :: Maybe Text
  , belongsToCollection :: Maybe Collection
  , budget :: Int
  , genres :: [Genre]
  , homepage :: Maybe Text
  , id :: Int
  , imdbId :: Maybe Text
  , originalLanguage :: Text
  , originalTitle :: Text
  , overview :: Maybe Text
  , popularity :: Float
  , posterPath :: Maybe Text
  , productionCompanies :: [ProductionCompany]
  , productionCountries :: [ProductionCountry]
  , releaseDate :: Day
  , revenue :: Int
  , runtime :: Maybe Int
  , spokenLanguages :: [SpokenLanguage]
  , status :: MovieStatus
  , tagline :: Maybe Text
  , title :: Text
  , video :: Bool
  , voteAverage :: Float
  , voteCount :: Int
  , videos :: Maybe VideoAppendix
  , images :: Maybe ImageAppendix
  }
  deriving stock (Eq, Generic)
  deriving (Show) via PrettyJSON MovieDetail
  deriving (FromJSON, ToJSON) via Snake MovieDetail

makeFieldLabelsNoPrefix ''MovieDetail