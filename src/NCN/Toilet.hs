{-# LANGUAGE OverloadedStrings, FlexibleContexts #-}
module NCN.Toilet
  ( toilets
  , uuid
  , location
  , name
  , inCircle
  , module NCN.Toilet.Types
  ) where
import Control.Applicative
import Control.Monad.Trans
import Control.Monad.Trans.Control
import Database.MongoDB as M
import NCN.Toilet.Types

-- | Collection of toilets
toilets :: Collection
toilets = "toilets"

uuidIs :: UUID -> Field
uuidIs = ("uuid" =:)

locationIs :: Location -> Field
locationIs = ("location" =:)

nameIs :: Name -> Field
nameIs = ("name" =:)

-- | Query for Toilets within specified circle
inCircle :: (Select s) => Double -> Location -> s
inCircle radius center = select
  [ location =:
    [ "$geoWithin" =:
      ["$center" =: [val [longitude center, latitude center], val radius]]
    ]
  ] toilets

