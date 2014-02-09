{-# LANGUAGE TemplateHaskell #-}
module NCN.Config.FromJSON(module NCN.Config) where
import Control.Applicative
import Data.Aeson
import Data.Aeson.TH
import Data.Char
import Data.Text(unpack)
import Database.MongoDB
import NCN.Config

instance FromJSON Host where
  parseJSON  = withText "Host" $ pure . readHostPort . unpack

deriveFromJSON defaultOptions ''Config
deriveFromJSON (defaultOptions{fieldLabelModifier = \s -> case drop (length "server") s of (c:cs) -> toLower c : cs }) ''ServerConfig
deriveFromJSON (defaultOptions{fieldLabelModifier = \s -> case drop (length "mongoDB") s of (c:cs) -> toLower c : cs }) ''MongoDBConfig
