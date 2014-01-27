{-# LANGUAGE TemplateHaskell #-}
module NCN.Config where

import Data.Yaml
import Data.Aeson.TH
import NCN.Config.Database

data Config = Config
  { server :: ServerConfig
  , database :: DatabaseConfig
  } deriving (Show)

data ServerConfig = ServerConfig
  { host :: String
  , port :: Int
  } deriving(Show)

deriveFromJSON defaultOptions ''Config
deriveFromJSON defaultOptions ''ServerConfig
