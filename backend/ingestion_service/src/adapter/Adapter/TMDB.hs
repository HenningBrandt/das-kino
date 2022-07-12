module Adapter.TMDB (
  adaptTMDB,
) where

import Data.Aeson (FromJSON)
import Data.ByteString (ByteString)
import Data.Maybe (fromJust)
import Data.Text (Text)
import Domain.Ingestion (Config)
import Network.HTTP.Req (
  GET (..),
  NoReqBody (..),
  Option,
  Scheme (Https),
  Url,
  (/:),
  (/~),
  (=:),
 )
import qualified Network.HTTP.Req as Req
import Optics
import Polysemy (Embed, Members, Sem, embed, interpret)
import Polysemy.Reader (Reader)
import qualified Polysemy.Reader as Reader
import Port.TMDB (TMDB)
import qualified Port.TMDB as TMDB

apiVersion :: Text
apiVersion = "3"

adaptTMDB :: Members '[Embed IO, Reader Config] r => Sem (TMDB : r) a -> Sem r a
adaptTMDB = interpret $ \eff -> do
  config <- Reader.ask @Config
  let baseUrl =
        (/: apiVersion) . fst . fromJust $
          Req.useHttpsURI (config ^. #tmdbBaseUrl % #uri)
  let apiToken = config ^. #tmdbToken % #bytes
  let langParam = ("language" =: ("de" :: Text) :: Option 'Https)
  case eff of
    TMDB.SearchMovies query -> embed $ do
      let url = baseUrl /: "search" /: "movie"
      let queryParam = "query" =: query
      runRequest url apiToken (langParam <> queryParam)
    TMDB.GetMovieDetail id -> embed $ do
      let url = baseUrl /: "movie" /~ id
      let appendParam = "append_to_response" =: ("videos,images" :: Text)
      runRequest url apiToken (langParam <> appendParam)
    TMDB.GetConfiguration -> embed $ do
      let url = baseUrl /: "configuration"
      runRequest url apiToken mempty

runRequest ::
  FromJSON a =>
  Url 'Https ->
  ByteString ->
  Option 'Https ->
  IO (Either () a)
runRequest url token options = Req.runReq Req.defaultHttpConfig $ do
  let auth = Req.oAuth2Bearer token
  response <- Req.req GET url NoReqBody Req.jsonResponse (auth <> options)
  case Req.responseStatusCode response of
    200 -> return . Right . Req.responseBody $ response
    _ -> return . Left $ ()