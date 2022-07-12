module Main (
  main,
) where

import Test.Hspec

import qualified DasKinoScraperSpec as DasKinoScraper
import qualified TMDBClientSpec as TMDBClient

main :: IO ()
main = hspec $ do
  DasKinoScraper.spec
  TMDBClient.spec