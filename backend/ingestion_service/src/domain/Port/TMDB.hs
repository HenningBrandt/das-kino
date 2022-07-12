{-# LANGUAGE TemplateHaskell #-}

module Port.TMDB (
  TMDB (..),
  Page (..),
  Movie (..),
  MoviePage,
  MovieDetail (..),
  Collection (..),
  Genre (..),
  ProductionCompany (..),
  ProductionCountry (..),
  SpokenLanguage (..),
  MovieStatus (..),
  VideoAppendix (..),
  Video (..),
  ImageAppendix (..),
  Image (..),
  Configuration (..),
  searchMovies,
  getMovieDetail,
  getConfiguration,
) where

import Data.Text (Text)
import Polysemy (makeSem)
import Port.DTO.Collection (Collection (..))
import Port.DTO.Configuration (Configuration)
import Port.DTO.Genre (Genre (..))
import Port.DTO.Image (Image (..))
import Port.DTO.ImageAppendix (ImageAppendix (..))
import Port.DTO.Movie (Movie (..))
import Port.DTO.MovieDetail (MovieDetail (..))
import Port.DTO.MovieStatus (MovieStatus (..))
import Port.DTO.Page (Page (..))
import Port.DTO.ProductionCompany (ProductionCompany (..))
import Port.DTO.ProductionCountry (ProductionCountry (..))
import Port.DTO.SpokenLanguage (SpokenLanguage (..))
import Port.DTO.Video (Video (..))
import Port.DTO.VideoAppendix (VideoAppendix (..))

-- TODO: Make movie id typesafe
-- Get youtube thumbnail: https://i.ytimg.com/vi/{video_id}/hqdefault.jpg
-- TODO: What other video sources are there and how to get their thumbnail

type MoviePage = Page Movie

data TMDB m a where
  SearchMovies :: Text -> TMDB m (Either () MoviePage)
  GetMovieDetail :: Int -> TMDB m (Either () MovieDetail)
  GetConfiguration :: TMDB m (Either () Configuration)
makeSem ''TMDB
