{-# LANGUAGE TemplateHaskell #-}
module NCN.Config(Config(..), ServerConfig(..), MongoDBConfig(..)) where
import Control.Applicative
import Data.Char
import Data.Text(unpack)
import Data.Aeson
import Data.Aeson.TH
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
  } deriving (Show)

instance FromJSON Host where
  parseJSON  = withText "Host" $ pure . readHostPort . unpack

deriveFromJSON defaultOptions ''Config
deriveFromJSON (defaultOptions{fieldLabelModifier = \s -> case drop (length "server") s of (c:cs) -> toLower c : cs }) ''ServerConfig
deriveFromJSON (defaultOptions{fieldLabelModifier = \s -> case drop (length "mongoDB") s of (c:cs) -> toLower c : cs }) ''MongoDBConfig
