{-# LANGUAGE OverloadedStrings, LambdaCase, DeriveDataTypeable #-}
module NCN.Toilet.Types where
import Control.Applicative
import Control.Monad
import Data.ByteString.Lazy(toStrict, fromStrict)
import Data.Bson as B
import Data.Text
import Data.Typeable
import qualified Data.UUID as U

class ToDocument a where
  toDocument :: a -> B.Document

class FromDocument a where
  fromDocument :: B.Document -> Maybe a

data Toilet = Toilet
  { uuid :: U.UUID
  , location :: Location
  } deriving (Eq, Show)

instance ToDocument Toilet where
  toDocument = (<*>) [("uuid" =:) . uuid,  ("location" =:) . location] . pure

instance FromDocument Toilet where
  fromDocument d = Toilet <$> B.lookup "uuid" d <*> B.lookup "location" d

data Location = Location
  { longitude :: Double
  , latitude :: Double
  } deriving (Eq, Show, Typeable)

instance ToDocument Location where
  toDocument (Location lo la) =
    [ "type" =: ("Point"::Text)
    , "coordinates" =: [lo, la]
    ]
instance FromDocument Location where
    fromDocument d = do
      B.lookup "type" d >>= guard . (("Point" :: Text) ==)
      B.lookup "coordinates" d >>= \case
        [lo, la] -> return $ Location lo la
        _        -> Nothing

instance B.Val Location where
  val = B.val . toDocument
  cast' = B.cast' >=> fromDocument

instance B.Val U.UUID where
  val = B.val . B.UUID . toStrict . U.toByteString
  cast' = B.cast' >=> \case (B.UUID b) -> U.fromByteString (fromStrict b)
