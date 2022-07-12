module Domain.Ingestion (
  Token (..),
  Config (..),
  ingest,
) where

import Control.Monad (forM_)
import Data.Aeson (FromJSON, ToJSON)
import qualified Data.Aeson as Aeson
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as LBS
import Data.Maybe (fromJust, fromMaybe)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TextEncoding
import Data.Time (Day, TimeZone, UTCTime (..))
import qualified Data.Time.Zones.DB as TZonesDB
import Deriving.Aeson (CustomJSON (..))
import Deriving.Aeson.Stock (Snake)
import Domain.Configuration (Config)
import qualified Domain.Enrichement as Enrich
import Domain.Movie (UTCMovie)
import qualified Domain.Movie as Movie
import qualified Domain.Scraper as Scraper
import GHC.Generics (Generic)
import Optics
import Overture.Data (Token, URL)
import Overture.Deriving (PrettyJSON (..))
import Polysemy (Member, Members, Sem)
import Polysemy.Error (Error)
import Polysemy.Reader (Reader)
import qualified Polysemy.Reader as Reader
import Port.Clock (Clock)
import qualified Port.Clock as Clock
import Port.Database (Database)
import qualified Port.Database as DB
import Port.HTTP (HTTP)
import qualified Port.HTTP as HTTP
import Port.TMDB (TMDB)
import qualified Port.TMDB as TMDB
import Text.URI (URI)
import qualified Text.URI as URI

-- Services for getting html, time conversions, db storage, timdb client, message queue
-- Steps:
--    Getting html for this and next week
--    Scrape movies
--    Convert times to UTC
--    Enrich via TMDB
--      Logic to extract most likely movie from search
--    Assemble media links
--    Store in db
--    Enqueue update message

-- Cache
--    movie name -> tmdb data
--    tmbd config (invalidation strategy?)

-- Failure scenarios
--    Fetching html failed => Logging && Retry procedure
--    Scrape movies fails => Log failure and html
--    TMDB config fails => Logging && Retry procedure
--    TMDB search/details fails => Logging && Retry procedure
--    Database not accessible => Logging && Retry procedure
--    Enqueue fails => Logging && Retry procedure

ingest ::
  Members '[Clock, HTTP, TMDB, Database, Reader Config, Error ()] r =>
  Sem r Text
ingest = do
  config <- Reader.ask @Config
  UTCTime today _ <- Clock.getCurrentTime
  berlinTime <- Clock.getTimeZone TZonesDB.Europe__Berlin

  -- Scrape current and upcoming showtimes from "Das Kino" and enrich them
  -- with data from "The Movie Database"
  movies <-
    Movie.mergeShowTimes
      <$> getMovies (config ^. #currentUrl % #uri) today berlinTime
      <*> getMovies (config ^. #upcomingUrl % #uri) today berlinTime
      >>= mapM Enrich.enrichMovie

  -- Load most recent scrape of the same movies to compare them later
  -- and enqueue update messages if needed
  latestMovies <- DB.fetchLatestMovie (movies ^.. each % #movie % #name)

  -- Update the database with the current scraping results
  forM_ movies $ \m -> do
    DB.insertMovie (m ^. #movie) (m ^? #tmdbData % _Just % #id)
    forM_ (m ^. #tmdbData) DB.upsertMovieDetail

  -- Update TMDB configuration
  -- TODO: Log error
  TMDB.getConfiguration >>= mapM_ DB.upsertConfiguration

  -- TODO: Compare latest and new movies and enqueue update messages

  return . Text.pack . show $ movies

getMovies :: Member HTTP r => URI -> Day -> TimeZone -> Sem r [UTCMovie]
getMovies uri today timeZone = do
  HTTP.getURI uri >>= \case
    Left _ -> return [] -- TODO: Log error
    Right html ->
      return
        . Movie.convertToUTC timeZone today
        . fromMaybe []
        . Scraper.scrapeMovies
        $ TextEncoding.decodeUtf8 html