{-# LANGUAGE OverloadedStrings #-}
module NCN.Database
  ( doAction
  , doActionM
  , module Database.MongoDB
  ) where

import Control.Applicative(liftA2)
import Control.Monad.Error.Class
import Control.Monad.IO.Class(liftIO)
import Control.Monad.Reader
import Data.Either
import Database.MongoDB
import NCN.Config.Database as C
import Web.Scotty.Trans(ScottyError(...))

instance ScottyError Failure where
  stringError = strMsg
  showError = show

raiseLeft :: (ScottyError e, Monad m) => m (Either e a) -> ActionT e m a
raiseLeft m = do
  either <- lift m
  case either of
    Left e -> raise e
    Right a -> return a

doAction :: Action m a -> ReaderT C.DatabaseConfig m (Either Failure a)
doAction action = do
  hostPort <- asks $ readHostPort . (liftA2 (++)  C.host ((':':) . C.port))
  db <- asks C.database
  up <- asks $ liftA2 (,) C.user C.pass
  lift $ do
    pipe <- runIOE $ connect hostPort
    result <- access pipe master db ((uncurry auth up) >> action)
    close pipe
    return result

tokens = "tokens"
users = "users"
