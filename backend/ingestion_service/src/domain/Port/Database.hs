{-# LANGUAGE TemplateHaskell #-}

module Port.Database (
  Database (..),
  upsertConfiguration,
  upsertMovieDetail,
  insertMovie,
  fetchLatestMovie,
) where

import Data.Text (Text)
import qualified Domain.Movie as Domain
import Polysemy (makeSem)
import qualified Port.TMDB as TMDB

data Database m a where
  UpsertConfiguration :: TMDB.Configuration -> Database m ()
  UpsertMovieDetail :: TMDB.MovieDetail -> Database m ()
  InsertMovie :: Domain.UTCMovie -> Maybe Int -> Database m ()
  FetchLatestMovie :: [Text] -> Database m [Domain.UTCMovie]
makeSem ''Database
