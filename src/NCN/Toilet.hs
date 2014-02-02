{-# LANGUAGE OverloadedStrings, FlexibleContexts #-}
module NCN.Toilet where
import Control.Applicative
import Control.Monad.Trans
import Control.Monad.Trans.Control
import Database.MongoDB as M
import NCN.Toilet
import NCN.Toilet.Types
import NCN.MongoDB

toilets :: Collection
toilets = "toilets"

insert :: (MonadIO m, Applicative m, Functor m) => ToiletLocation -> Action m ()
insert = M.insert_ collection . toDocument

delete :: MonadIO m => ToiletId -> Action m ()
delete id = deleteOne $ select ["uuid" =: id] toilets

findInCircle :: (MonadIO m, MonadBaseControl IO m) => Double -> Location -> Action m Cursor
findInCircle radius center = find query where
  query = select
    [ "location" =:
      [ "$geoWithin" =:
        ["$center" =: [val [longitude center, latitude center], val radius]]
      ]
    ] toilets

getLocation :: MonadIO m => ToiletId -> Action m (Maybe Location)
getLocation id =  findOne query >>= return . (>>=M.lookup "location")
  where query = (select ["uuid" =: id] toilets){project = ["location" =: (1::Int)]}

