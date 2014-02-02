{-# LANGUAGE OverloadedStrings, LambdaCase, DeriveDataTypeable, TemplateHaskell #-}
module NCN.Toilet.Types
  ( Location(..)
  , Name
  , module U
  ) where
import Control.Monad
import Data.Aeson.TH
import Data.ByteString.Lazy(toStrict, fromStrict)
import Data.Bson as B
import Data.Text
import Data.Typeable
import Data.UUID as U

type Name = Text

data Location = Location
  { longitude :: Double
  , latitude :: Double
  } deriving (Eq, Show, Typeable)

deriveJSON defaultOptions ''Location

instance Val Location where
  val (Location lo la) = val
    [ "type" =: ("Point"::Text)
    , "coordinates" =: [lo, la]
    ]
  cast' = cast' >=> \d -> do
    B.lookup "type" d >>= guard . (("Point" :: Text) ==)
    B.lookup "coordinates" d >>= \case
      [lo, la] -> return $ Location lo la
      _        -> Nothing

instance Val U.UUID where
  val = val . B.UUID . toStrict . U.toByteString
  cast' = cast' >=> \case (B.UUID b) -> U.fromByteString (fromStrict b)

