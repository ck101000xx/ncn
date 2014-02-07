{-# LANGUAGE OverloadedStrings #-}
module Main where
import Control.Monad.Reader
import Data.Yaml
import NCN.App
import NCN.Config
import Web.Scotty.Trans

main = do
  decodeFileEither "./cfg.yaml" >>= either (putStrLn . show) (runReaderT $ ncnScottyT handleToilets)

ncnScottyT :: ScottyT e (ReaderT Config IO) () -> ReaderT Config IO ()
ncnScottyT s = do
  port <- asks (serverPort . server)
  cfg <- ask
  scottyT port id (flip runReaderT cfg) s

{- 
ncnScottyAppT :: (Monad m) => ScottyT (ReaderT Config m) () -> ReaderT Config m Application
ncnScottyAppT = do
  cfg <- ask
  scottyAppT id (runReaderT cfg)
-}
