{-# LANGUAGE OverloadedStrings #-}
module NCN.Database
  ( doAction
  , doActionM
  , module Database.MongoDB
  ) where

import Control.Applicative(liftA2)
import Control.Monad.Error
import Control.Monad.IO.Class(liftIO)
import Control.Monad.Reader
import Data.Either
import Database.MongoDB
import NCN.Config.Database as C
import Web.Scotty.Trans(ScottyError(...))

instance ScottyError Failure where
  stringError = strMsg
  showError = show

doAction :: Action m a -> ReaderT C.DatabaseConfig (ErrorT Failure m) a
doAction action = do
  hostPort <- asks $ readHostPort . (liftA2 (++)  C.host ((':':) . C.port))
  db <- asks C.database
  up <- asks $ liftA2 (,) C.user C.pass
  lift $ do
    lift $ do
      pipe <- runIOE $ connect hostPort
      result <- access pipe master db ((uncurry auth up) >> action)
      close pipe
    ErrorT result

tokens = "tokens"
users = "users"
