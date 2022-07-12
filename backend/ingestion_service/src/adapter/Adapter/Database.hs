{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module Adapter.Database (
  Connection,
  connect,
  adaptDatabase,
) where

import Control.Monad (void)
import Data.Aeson (FromJSON, ToJSON)
import qualified Data.Aeson as Aeson
import Data.Aeson.Key (Key)
import qualified Data.AesonBson as Bson
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import Data.Time (UTCTime, getCurrentTime)
import Database.MongoDB ((=:))
import qualified Database.MongoDB as Mongo
import Deriving.Aeson (CustomJSON (..))
import Deriving.Aeson.Stock (Snake)
import Domain.Configuration (Config)
import GHC.Generics (Generic)
import Optics
import Overture.Data (urlToString)
import Overture.Deriving (PrettyJSON (..))
import Overture.Either (rightToMaybe)
import Overture.Monad (forMaybeM)
import Polysemy (Embed, Member, Sem, embed, interpret)
import Port.Database (Database)
import qualified Port.Database as DB

class ToDocument a where
  toDocument :: a -> Mongo.Document

class FromDocument a where
  fromDocument :: Mongo.Document -> Maybe a

newtype JSONDocument a = JSONDocument a

instance (ToJSON a) => ToDocument (JSONDocument a) where
  toDocument (JSONDocument wrapped) = case Aeson.toJSON wrapped of
    Aeson.Object obj -> Bson.bsonifyError obj
    _ -> error "Only JSON objects are convertible to BSON documents"

instance (FromJSON a) => FromDocument (JSONDocument a) where
  fromDocument doc = case Aeson.fromJSON (Aeson.Object (Bson.aesonify doc)) of
    Aeson.Success wrapped -> Just (JSONDocument wrapped)
    Aeson.Error _ -> Nothing

newtype Connection = Connection Mongo.Pipe

data WithTimestamps a = WithTimestamps
  { creationTimestamp :: UTCTime
  , updatedTimestamp :: UTCTime
  , wrapped :: a
  }
  deriving stock (Eq, Generic)
  deriving (Show) via PrettyJSON (WithTimestamps a)
  deriving (FromJSON, ToJSON) via Snake (WithTimestamps a)
  deriving (FromDocument, ToDocument) via JSONDocument (WithTimestamps a)

type ForeignKeys = HashMap Text Text

data WithForeignKeys a = WithForeignKeys
  { foreignKeys :: ForeignKeys
  , wrapped :: a
  }
  deriving stock (Eq, Generic)
  deriving (Show) via PrettyJSON (WithForeignKeys a)
  deriving (FromJSON, ToJSON) via Snake (WithForeignKeys a)
  deriving (FromDocument, ToDocument) via JSONDocument (WithForeignKeys a)

makeFieldLabelsNoPrefix ''WithTimestamps
makeFieldLabelsNoPrefix ''WithForeignKeys

type Entity a = WithTimestamps (WithForeignKeys a)

toEntity :: UTCTime -> UTCTime -> ForeignKeys -> a -> Entity a
toEntity created updated foreignKeys wrapped =
  WithTimestamps created updated (WithForeignKeys foreignKeys wrapped)

fromEntity :: Entity a -> a
fromEntity (WithTimestamps _ _ (WithForeignKeys _ wrapped)) = wrapped

connect :: Config -> IO Connection
connect config =
  Connection
    <$> Mongo.connect (Mongo.host . urlToString $ (config ^. #databaseUrl))

adaptDatabase ::
  Member (Embed IO) r =>
  Connection ->
  Sem (Database : r) a ->
  Sem r a
adaptDatabase connection = interpret $ \case
  DB.UpsertConfiguration _ -> return ()
  DB.UpsertMovieDetail _ -> return ()
  DB.InsertMovie movie detailId ->
    embed $ insert connection moviesCollection HashMap.empty movie -- TODO: Pass detailId as foreign key
  DB.FetchLatestMovie titles ->
    embed $
      forMaybeM titles $ \title ->
        fetchFirst connection (mostRecentByTitle title)

insert :: ToJSON a => Connection -> Mongo.Collection -> ForeignKeys -> a -> IO ()
insert connection collection foreignKeys value = do
  now <- getCurrentTime
  let entity = toEntity now now foreignKeys value
  void $ performAction connection (Mongo.insert collection (toDocument entity))

fetchFirst :: forall a. FromJSON a => Connection -> Mongo.Query -> IO (Maybe a)
fetchFirst connection query = do
  entity <- performAction connection (Mongo.findOne query)
  return (entity >>= (fmap fromEntity . fromDocument @(Entity a)))

performAction :: Connection -> Mongo.Action IO a -> IO a
performAction (Connection pipe) = Mongo.access pipe Mongo.master "main"

-- TODO: Fix query to accommodate Entity structure
mostRecentByTitle :: Text -> Mongo.Query
mostRecentByTitle title =
  (Mongo.select ["title" =: title] moviesCollection)
    { Mongo.sort = ["entity_meta_creation_timestamp" =: (-1 :: Int)]
    }

moviesCollection :: Mongo.Collection
moviesCollection = "movies"
