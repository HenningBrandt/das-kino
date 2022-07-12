module Domain.PushNotification (
  PushNotification (..),
  updateNotifications,
) where

import Data.Text (Text)
import Domain.Enrichement (EnrichedMovie)
import Domain.Movie (UTCMovie)

newtype PushNotification = PushNotification Text

updateNotifications :: [UTCMovie] -> [EnrichedMovie] -> [PushNotification]
updateNotifications = undefined