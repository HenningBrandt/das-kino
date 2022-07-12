{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}

module Main where

import Adapter.Clock (adaptClock)
import Adapter.Database (Connection, adaptDatabase, connect)
import Adapter.HTTP (adaptHTTP)
import Adapter.TMDB (adaptTMDB)
import Data.Either (fromRight)
import Data.Function ((&))
import Data.Maybe (fromJust)
import Data.Text (Text)
import qualified Data.Text.Encoding as TextEncoding
import qualified Data.Text.IO as Text
import Dhall (FromDhall (..))
import qualified Dhall
import Domain.Ingestion (Config (..), ingest)
import Overture.Data (Token (..), URL (..))
import Polysemy (runM)
import Polysemy.Error (runError)
import Polysemy.Reader (runReader)
import qualified Text.URI as URI

instance FromDhall Token where
  autoWith _ = fmap (Token . TextEncoding.encodeUtf8) Dhall.strictText

instance FromDhall URL where
  autoWith _ = fmap (URL . fromJust . URI.mkURI) Dhall.strictText

deriving anyclass instance FromDhall Config

main :: IO ()
main = do
  config <- Dhall.input Dhall.auto "./config.dhall"
  connection <- connect config
  result <- adaptPorts config connection ingest
  Text.putStr (fromRight "Error" result)

adaptPorts :: Config -> Connection -> _ -> IO (Either () Text)
adaptPorts config connection =
  runM
    . runError @()
    . runReader config
    . adaptTMDB
    . adaptHTTP
    . adaptClock
    . adaptDatabase connection
