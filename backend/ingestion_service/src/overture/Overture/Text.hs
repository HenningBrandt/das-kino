module Overture.Text (
  charListToText,
) where

import Data.Text (Text)
import qualified Data.Text as Text

charListToText :: [Char] -> Text
charListToText = foldMap Text.singleton