module TMDBClientSpec (
  spec,
) where

import Control.Monad (forM_)
import Data.Aeson
import qualified Data.ByteString.Lazy as LBS
import qualified Port.TMDB as TMDB
import System.Directory (getCurrentDirectory)
import System.FilePath ((</>))
import Test.Hspec

spec :: Spec
spec =
  describe "decoding of TMDB responses" $ do
    forM_
      [ "the_batman_search.json"
      , "raiders_of_the_lost_ark_search.json"
      , "everything_everywhere_search.json"
      ]
      $ \fileName ->
        it ("decodes movie search: " <> fileName) $ do
          withResource fileName $ \jsonData ->
            case eitherDecode @TMDB.MoviePage jsonData of
              Left error -> expectationFailure (fileName <> ": " <> error)
              Right _ -> return ()
    forM_
      [ "the_batman_details.json"
      , "raiders_of_the_lost_ark_details.json"
      , "everything_everywhere_all_at_once_details.json"
      ]
      $ \fileName ->
        it ("decodes movie details: " <> fileName) $ do
          withResource fileName $ \jsonData ->
            case eitherDecode @TMDB.MovieDetail jsonData of
              Left error -> expectationFailure (fileName <> ": " <> error)
              Right _ -> return ()

withResource :: String -> (LBS.ByteString -> IO a) -> IO a
withResource fileName handler = do
  path <- (</> ("test" </> "unit_tests" </> "res" </> "tmdb" </> fileName)) <$> getCurrentDirectory
  contents <- LBS.readFile path
  handler contents