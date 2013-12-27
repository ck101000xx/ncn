{-# LANGUAGE OverloadedStrings #-}
module NCN.Auth(getNewToken, deleteToken, ensureUser) where

import Data.Maybe
import Data.Either

import Data.UUID(toString)
import Data.UUID.V4

import NCN.Database(doAction, tokens)
import NCN.Types

import Control.Monad.Reader
import Data.Digest.Pure.SHA(bytestringDigest, sha1)

type RDI a = ReaderT DatabaseConfig IO (Either Failure a)

hash = bytestringDigest . sha1

getNewToken :: String -> RDI Token
getNewToken email = doAction $ do
  token <- liftIO newToken
  insert tokens ["hash" =: hash token, "email" =: email]
  return token
  where newToken = toString . nextRandom

deleteToken :: Token -> RDI ()
deleteToken token = doAction $ delete tokens (select tokens ["hash" =: hash token])

ensureUser :: Email -> RDI ()
ensureUser email = doAction $ do
  ensureIndex $ (index users [email =: 1]){iUnique = True}
  insert_ ["email" =: email]

getEmail :: Token -> RDI (Maybe Email)
getEmail = doAction $ findOne (select tokens ["email" =: email])