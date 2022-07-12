module Domain.Scraper (
  scrapeMovies,
) where

import Control.Applicative ((<|>))
import Data.Functor ((<&>))
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import qualified Data.Text as Text
import Domain.Movie (
  BookingLink (..),
  Date (..),
  Format (..),
  LocalTimeDate,
  LocalTimeMovie,
  LocalTimeShowTime,
  Movie (..),
  ShowTime (ShowTime),
 )
import Overture.Alternative (optional)
import Overture.Either (rightToMaybe)
import Overture.Text (charListToText)
import Overture.Time (dateFromString)
import Text.HTML.Scalpel (Scraper, (//), (@:))
import qualified Text.HTML.Scalpel as Scalpel
import Text.Parsec (Parsec)
import qualified Text.Parsec as Parsec
import Text.URI (URI)
import qualified Text.URI as URI

scrapeMovies :: Text -> Maybe [LocalTimeMovie]
scrapeMovies = flip Scalpel.scrapeStringLike $ do
  Scalpel.chroots ("div" @: [Scalpel.hasClass "dk_program_container"]) movieScraper
 where
  movieScraper :: Scraper Text LocalTimeMovie
  movieScraper = do
    format <- formatScraper
    Movie <$> nameScraper <*> showTimesScraper format

  nameScraper :: Scraper Text Text
  nameScraper =
    Scalpel.text ("span" @: [Scalpel.hasClass "dk_movieTitle"])
      <&> Text.unwords . Text.words . Text.replace "(3D)" ""

  formatScraper :: Scraper Text Format
  formatScraper =
    Scalpel.text ("span" @: [Scalpel.hasClass "dk_movieTitle"])
      <&> (\name -> if "3D" `Text.isInfixOf` name then Format3D else Format2D)

  showTimesScraper :: Format -> Scraper Text [LocalTimeShowTime]
  showTimesScraper format =
    Scalpel.chroot ("table" @: [Scalpel.hasClass "dk_showingTable"]) $ do
      params <- zipWith toShowTimeParams <$> dayScraper <*> showTimeTableScraper
      return . mapMaybe (showTimeFromParams format) . concat $ params

  toShowTimeParams :: Text -> [(Text, Maybe Text)] -> [(Maybe LocalTimeDate, Maybe URI)]
  toShowTimeParams day row = row <&> \(time, link) -> (parseDate (day <> time), link >>= URI.mkURI)

  showTimeFromParams :: Format -> (Maybe LocalTimeDate, Maybe URI) -> Maybe LocalTimeShowTime
  showTimeFromParams format (date, uri) = date <&> \d -> ShowTime d format (BookingLink <$> uri)

  showTimeTableScraper :: Scraper Text [[(Text, Maybe Text)]]
  showTimeTableScraper =
    Scalpel.chroot ("tbody" @: [Scalpel.hasClass "dk_showBody"]) $
      Scalpel.chroots "tr" . Scalpel.chroots "td" $ do
        time <- Scalpel.text "span"
        link <- optional $ Scalpel.attr "href" "a"
        return (time, link)

  dayScraper :: Scraper Text [Text]
  dayScraper = Scalpel.chroot ("tr" @: [Scalpel.hasClass "dk_showDays"]) (Scalpel.texts "th")

type Parser = Parsec Text ()

parseDate :: Text -> Maybe LocalTimeDate
parseDate = rightToMaybe . Parsec.parse dateRowParser "date row"
 where
  dateRowParser :: Parser LocalTimeDate
  dateRowParser = todayParser <|> dateTimeParser

  todayParser :: Parser LocalTimeDate
  todayParser = do
    timeStr <- Parsec.string "HEUTE" *> skipPrefix timeParser
    Today <$> dateFromString "%H:%M" timeStr

  dateTimeParser :: Parser LocalTimeDate
  dateTimeParser = do
    dateStr <- (<>) <$> skipPrefix dateParser <*> skipPrefix timeParser
    Date <$> dateFromString "%d-%m-%Y%H:%M" dateStr

  -- Format: 24.05.2022
  dateParser :: Parser Text
  dateParser =
    (\day month year -> day <> "-" <> month <> "-" <> year)
      <$> digits 2 <* Parsec.char '-' <*> digits 2 <* Parsec.char '-' <*> digits 4

  -- Format: 20:00
  timeParser :: Parser Text
  timeParser =
    (\hrs mins -> hrs <> ":" <> mins)
      <$> digits 2 <* Parsec.char ':' <*> digits 2

  skipPrefix :: Parser a -> Parser a
  skipPrefix p = Parsec.try p <|> (Parsec.anyChar *> skipPrefix p)

  digits :: Int -> Parser Text
  digits n = charListToText <$> Parsec.count n Parsec.digit
