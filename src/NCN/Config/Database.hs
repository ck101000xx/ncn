{-# LANGUAGE OverloadedStrings #-}
module NCN.Config.Database where

import Control.Applicative
import Data.Yaml

data DatabaseConfig = DatabaseConfig
  { host::String
    port::Int
    user::String
    pass::String
  } deriving (Show)

instance FromJSON DatabaseConfig where
  parseJSON (Object v) =
    DatabaseConfig <$>
    v .: "host" <*>
    v .: "port" <*>
    v .: "user" <*>
    v .: "pass"
    
