{-# LANGUAGE OverloadedStrings #-}
import Web.Scotty

import NCN.Auth
import NCN.Types
import qualified NCN.Config as C
import Control.Monad.Reader

import Network.HTTP.Conduit(newManager)

main = do
  maybeConfig <- decodeFile ""
  case maybeConfig of
    Just Config -> app config
    Nothing -> putStrLn "Invalid config"
  
app config = scotty 3000 $ do
  get "/" $ do
    html "<h1>Hello World</h1>"
  post "/tokens" $ do
    maybeEmail <- param "assertion" >>= liftIO getEmail
    case maybeEmail of
      Just email ->
        token <- liftIO . runReaderT (C.acid config) $ do
          ensureUser email
          getNewToken email
        text token
      Nothing -> status status401
  delete "/tokens/:t" $ do
    a <- getEmail
    param "t" >>= liftIO . runReaderT (C.mongoDB config) $ deleteToken t
  where
    auth = do
      token <- param "token"
      
checkAss assertion = newManager def >>= checkAssertion audience assertion
  where
    audience = "http://ck101000xx.kd.io"