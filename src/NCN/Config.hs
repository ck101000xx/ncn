module NCN.Config
  ( Config(..)
  , ServerConfig(..)
  , MongoDBConfig(..)
  ) where
import Database.MongoDB

data Config = Config
  { server :: ServerConfig
  , mongodb :: MongoDBConfig
  } deriving (Show)

data ServerConfig = ServerConfig
  { serverHost :: Maybe String
  , serverPort :: Int
  } deriving (Show)

data MongoDBConfig = MongoDBConfig
  { mongoDBHost :: Host
  , mongoDBDatabase :: Database
  , mongoDBUsername :: Maybe Username
  , mongoDBPassword :: Maybe Password
  } deriving (Show)

