{-# LANGUAGE TemplateHaskell #-}

module Port.Clock (
  Clock (..),
  getCurrentTime,
  getTimeZone,
) where

import Data.Time (TimeZone, UTCTime)
import Data.Time.Zones.DB (TZLabel)
import Polysemy (makeSem)

data Clock m a where
  GetCurrentTime :: Clock m UTCTime
  GetTimeZone :: TZLabel -> Clock m TimeZone
makeSem ''Clock
