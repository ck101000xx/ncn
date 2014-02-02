{-# LANGUAGE OverloadedStrings, FlexibleContexts #-}
module NCN.Toilet
  ( toilets
  , uuid
  , location
  , inCircle
  , module NCN.Toilet.Types
  ) where
import Control.Applicative
import Control.Monad.Trans
import Control.Monad.Trans.Control
import Database.MongoDB as M
import NCN.Toilet
import NCN.Toilet.Types

-- | Collection of toilets
toilets :: Collection
toilets = "toilets"

-- | UUID of a toilet
uuid :: Field
uuid = "uuid"

-- | Location of a toilet
location :: Field
location = "location"

-- | Query for Toilets within specified circle
inCircle :: (Select s) => Double -> Location -> s
inCircle radius center = select
  [ location =:
    [ "$geoWithin" =:
      ["$center" =: [val [longitude center, latitude center], val radius]]
    ]
  ] toilets

