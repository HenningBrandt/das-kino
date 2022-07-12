{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Redundant <$>" #-}

module Domain.Enrichement (
  EnrichedMovie (..),
  enrichMovie,
) where

import Control.Monad (join)
import Data.Aeson (FromJSON, ToJSON)
import qualified Data.List as List
import qualified Data.Text as Text
import Data.Time (Day, UTCTime (..))
import qualified Data.Time as Time
import qualified Data.Time.Calendar.OrdinalDate as Calendar
import qualified Data.Time.Format as TimeFormat
import Deriving.Aeson (CustomJSON (..))
import Deriving.Aeson.Stock (Snake)
import Domain.Movie (UTCMovie)
import GHC.Generics (Generic)
import Optics
import Overture.Deriving (PrettyJSON (..))
import Overture.Either (rightToMaybe)
import Polysemy (Members, Sem)
import Port.Clock (Clock)
import qualified Port.Clock as Clock
import Port.TMDB (TMDB)
import qualified Port.TMDB as TMDB

data EnrichedMovie = EnrichedMovie
  { movie :: UTCMovie
  , tmdbData :: Maybe TMDB.MovieDetail
  }
  deriving stock (Eq, Generic)
  deriving (Show) via PrettyJSON EnrichedMovie
  deriving (FromJSON, ToJSON) via Snake EnrichedMovie

makeFieldLabelsNoPrefix ''EnrichedMovie

-- TODO: Log error
enrichMovie :: Members '[Clock, TMDB] r => UTCMovie -> Sem r EnrichedMovie
enrichMovie movie =
  TMDB.searchMovies (movie ^. #name) >>= \case
    Left _ -> return (EnrichedMovie movie Nothing)
    Right page -> EnrichedMovie movie <$> findMovieOn page
 where
  findMovieOn ::
    Members '[Clock, TMDB] r =>
    TMDB.MoviePage ->
    Sem r (Maybe TMDB.MovieDetail)
  findMovieOn page =
    bestMatch page <$> Clock.getCurrentTime >>= \case
      Nothing -> return Nothing
      Just movie -> rightToMaybe <$> TMDB.getMovieDetail (movie ^. #id)

bestMatch :: TMDB.MoviePage -> UTCTime -> Maybe TMDB.Movie
bestMatch page (UTCTime today _) = List.find valid (page ^. #results)
 where
  valid movie =
    not (movie ^. #adult)
      && yearOf (movie ^. #releaseDate) `List.elem` validReleaseYears
  yearOf = fst . Calendar.toOrdinalDate
  validReleaseYears = [yearOf today, yearOf today - 1]