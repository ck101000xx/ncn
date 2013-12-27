{-# LANGUAGE OverloadedStrings #-}
module NCN.Config
  ( decodeFile
  , Config
  , ServerConfig
  ) where

import Control.Applicative
import Data.Yaml
import System.IO(FilePath)
import NCN.Config.Database

data Config = Config
  { server :: ServerConfig
  , database :: DatabaseConfig
  } deriving (Show)

instance FromJSON Config where
  parseJSON (Object v) =
    Config <$>
    v .: "server" <*>
    v .: "database"

data ServerConfig = ServerConfig
  { host::String
  , port::Int
  } deriving(Show)

instance FromJSON ServerConfig where
  parseJSON (Object v) =
    ServerConfig <$>
    v .: "host" <*>
    v .: "port"