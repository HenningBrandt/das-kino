{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module Overture.Data (
  Token (..),
  URL (..),
  urlToText,
  urlToString,
) where

import Data.Aeson (ToJSON (toJSON))
import qualified Data.Aeson as Aeson
import Data.ByteString (ByteString)
import Data.Text (Text)
import qualified Data.Text as Text
import GHC.Generics (Generic)
import Optics
import Overture.Deriving (RedactedShow (..), RedactedToJSON (..))
import Text.URI (URI)
import qualified Text.URI as URI

newtype Token = Token
  {bytes :: ByteString}
  deriving stock (Eq, Generic)
  deriving (Show) via RedactedShow Token
  deriving (ToJSON) via RedactedToJSON Token

newtype URL = URL
  {uri :: URI}
  deriving stock (Eq)
  deriving newtype (Show)

instance ToJSON URL where
  toJSON = Aeson.String . URI.render . (^. #uri)

urlToText :: URL -> Text
urlToText = URI.render . uri

urlToString :: URL -> String
urlToString = Text.unpack . urlToText

makeFieldLabelsNoPrefix ''Token
makeFieldLabelsNoPrefix ''URL