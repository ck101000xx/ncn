{-# LANGUAGE OverloadedStrings, FlexibleContexts #-}
module NCN.Toilet.Location where
import Control.Applicative
import Control.Monad.Trans
import Control.Monad.Trans.Control
import Database.MongoDB as M
import NCN.Toilet.Types
import NCN.MongoDB

collection :: Collection
collection = "toiletLocation"

data ToiletLocation = ToiletLocation
  { uuid :: ToiletId
  , location :: Location
  } deriving (Eq, Show)

instance Entity ToiletLocation where
  toDocument = (<*>) [("uuid" =:) . uuid,  ("location" =:) . location] . pure
  fromDocument d = ToiletLocation <$> M.lookup "uuid" d <*> M.lookup "location" d

insert :: (MonadIO m, Applicative m, Functor m) => ToiletLocation -> Action m ()
insert = M.insert_ collection . toDocument

delete :: MonadIO m => ToiletId -> Action m ()
delete id = deleteOne $ select ["uuid" =: id] collection

findInCircle :: (MonadIO m, MonadBaseControl IO m) => Double -> Location -> Action m Cursor
findInCircle radius center = find query where
  query = select
    [ "location" =:
      [ "$geoWithin" =:
        ["$center" =: [val [longitude center, latitude center], val radius]]
      ]
    ] collection

getLocation :: MonadIO m => ToiletId -> Action m (Maybe Location)
getLocation id =  findOne query >>= return . (>>=M.lookup "location")
  where query = (select ["uuid" =: id] collection){project = ["location" =: (1::Int)]}

