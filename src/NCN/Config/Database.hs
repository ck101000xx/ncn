{-# LANGUAGE TemplateHaskell #-}
module NCN.Config.Database where

import Data.Aeson.TH
import Data.Yaml

data DatabaseConfig = DatabaseConfig
  { host :: String
  , port :: Int
  , user :: String
  , pass :: String
  } deriving (Show)

deriveFromJSON defaultOptions ''DatabaseConfig
