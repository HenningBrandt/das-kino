{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module Domain.Configuration (
  Config (..),
) where

import Data.Aeson (ToJSON)
import Deriving.Aeson (CustomJSON (..))
import Deriving.Aeson.Stock (Snake)
import GHC.Generics (Generic)
import Optics (makeFieldLabelsNoPrefix)
import Overture.Data (Token, URL)
import Overture.Deriving (PrettyJSON (..))

data Config = Config
  { currentUrl :: URL
  , upcomingUrl :: URL
  , tmdbBaseUrl :: URL
  , tmdbToken :: Token
  , databaseUrl :: URL
  }
  deriving stock (Eq, Generic)
  deriving (Show) via PrettyJSON Config
  deriving (ToJSON) via Snake Config

makeFieldLabelsNoPrefix ''Config
