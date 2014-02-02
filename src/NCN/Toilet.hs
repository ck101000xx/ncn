{-# LANGUAGE OverloadedStrings, FlexibleContexts #-}
module NCN.Toilet where
import Control.Applicative
import Control.Monad.Trans
import Control.Monad.Trans.Control
import Database.MongoDB as M
import NCN.Toilet
import NCN.Toilet.Types
import NCN.MongoDB

-- | Collection of toilets
toilets :: Collection
toilets = "toilets"

-- | UUID of a toilet
uuid :: Field
uuid = "uuid"

-- | Location of a toilet
location :: Field
location = "location"

-- | Create a new Toilet and return its UUID
newToilet :: Action m ToiletId
newToilet = do
  id <- liftIO $ nextRandom
  insert_ toilets [uuid =: id]
  return id

-- | Query for Toilets within specified circle
inCircle :: (Select s) => Double -> Location -> s
inCircle radius center = select
  [ location =:
    [ "$geoWithin" =:
      ["$center" =: [val [longitude center, latitude center], val radius]]
    ]
  ] toilets

