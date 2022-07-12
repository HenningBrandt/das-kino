module Overture.Deriving (
  PrettyJSON (..),
  RedactedShow (..),
  RedactedToJSON (..),
) where

import Data.Aeson (ToJSON (toJSON))
import qualified Data.Aeson as Aeson
import Data.Aeson.Encode.Pretty (encodePretty)
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TextEncoding

{- | Use this with DerivingVia to derive a Show instance based on
  the pretty printed JSON representation of the underlying type.
-}
newtype PrettyJSON a = PrettyJSON
  {prettyJSON :: a}

instance (ToJSON a) => Show (PrettyJSON a) where
  show =
    Text.unpack
      . TextEncoding.decodeUtf8
      . LBS.toStrict
      . encodePretty
      . prettyJSON

{- | Use this with DerivingVia to derive a Show instance that redacts
  the underlaying data. Useful for secret values like tokens and passwords,
  that should not show up accidentally in logs.
-}
newtype RedactedShow a = RedactedShow
  {redactedShow :: a}

instance Show (RedactedShow a) where
  show _ = "<REDACTED>"

{- | Use this with DerivingVia to derive a ToJSON instance that redacts
  the underlaying data. Useful for secret values like tokens and passwords,
  that should not show up accidentally in JSON serializations.
-}
newtype RedactedToJSON a = RedactedToJSON
  {redactedToJSON :: a}

instance ToJSON (RedactedToJSON a) where
  toJSON _ = Aeson.String "<REDACTED>"