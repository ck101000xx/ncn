{-# LANGUAGE OverloadedStrings, FlexibleContexts #-}
module NCN.Toilet where
import Control.Applicative
import Control.Monad
import Control.Monad.Trans
import Data.Conduit
import Data.Conduit.List as CL
import Data.UUID as U
import Data.UUID.V4
import Database.MongoDB
import NCN.Toilet.Types

toilets :: Collection
toilets = "toilets"

newToilet :: Conduit Location IO Toilet
newToilet = awaitForever $  (liftIO nextRandom >>=) . fmap yield . flip Toilet

insertToilets :: (MonadIO m, Applicative m, Functor m) => Sink Toilet (Action m) ()
insertToilets =  CL.map toDocument =$ sinkInsert toilets

deleteToilets :: MonadIO m => Sink U.UUID (Action m) ()
deleteToilets = CL.map (\u -> select ["uuid" =: u] toilets) =$ sinkDelete

findNearWithin :: (MonadIO m, MonadBaseControl IO m) => Double -> Location -> Source (Action m) Toilet
findNearWithin radius center = sourceFind query $= mapMaybe fromDocument
  where
    query = select
      [ "location" =: 
        [ "$near" =: [longitude center, latitude center]
        , "$maxDistance" =: radius
        ]
      ] toilets

sinkInsert :: (MonadIO m, Applicative m, Functor m) => Collection -> Sink Document (Action m) ()
sinkInsert coll = awaitForever $ lift . insert_ coll

sinkDelete :: MonadIO m => Sink Selection (Action m) ()
sinkDelete = awaitForever $ lift . delete

sourceFind :: (MonadIO m, MonadBaseControl IO m) => Query -> Source (Action m) Document
sourceFind query = do
  cursor <- lift $ find query
  addCleanup (\completed -> unless completed $ closeCursor cursor) $ do
    let yieldNext = do
          closed <- lift $ isCursorClosed cursor
          unless closed $ lift (next cursor) >>=  maybe (return ()) ((>> yieldNext) . yield)
    yieldNext
