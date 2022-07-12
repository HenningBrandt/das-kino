module DasKinoScraperSpec (
  spec,
) where

import qualified Data.Text as T
import qualified Data.Text.IO as T
import Domain.Scraper
import System.Directory (getCurrentDirectory)
import System.FilePath ((</>))
import Test.Hspec

spec :: Spec
spec =
  describe "showtime scraping" $ do
    it "scrapes showtimes for Jurassic World on homepage" $ do
      withResource "jurassic_world__expired-showtimes.html" $ \html ->
        scrapeMovies html `shouldBe` Nothing

withResource :: String -> (T.Text -> IO a) -> IO a
withResource fileName handler = do
  path <- (</> ("test" </> "unit_tests" </> "res" </> "das_kino" </> fileName)) <$> getCurrentDirectory
  contents <- T.readFile path
  handler contents