module Overture.Time (
  dateFromString,
) where

import Data.Text (Text)
import qualified Data.Text as Text
import Data.Time (ParseTime)
import qualified Data.Time as Time

dateFromString :: (MonadFail m, ParseTime t) => Text -> Text -> m t
dateFromString format =
  Time.parseTimeM True Time.defaultTimeLocale (Text.unpack format)
    . Text.unpack