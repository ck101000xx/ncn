{-# LANGUAGE OverloadedStrings #-}
module NCN.App where
import Control.Applicative
import Control.Monad.Error
import Control.Monad.Reader
import Control.Monad.Trans
import Data.Maybe
import Data.Text.Lazy hiding(find)
import Data.UUID.V4
import Database.MongoDB hiding(connect, access, UUID)
import NCN.Config
import NCN.MongoDB
import NCN.Toilet
import Network.HTTP.Types.Status
import Web.Scotty.Trans as ST

handleIndex ::  ScottyT Text (ReaderT Config IO) ()
handleIndex = do
  get "/" $ text "Hello World"

handleToilets :: ScottyT Text (ReaderT Config IO) ()
handleToilets = do
  post "/toilets" $ do
    allowOrigin
    location' <- Location <$> param "location[latitude]" <*> param "location[longitude]"
    name' <- param "name"
    let doc = [location =: (location' :: Location), name =: (name' :: Name)]
        run = do
          id <- liftIO nextRandom
          insert_ toilets $ (uuid =: id):doc
          return id
    id <- raiseET connect >>= fmap raiseET (access master run)
    json . toString $ id
  get "/toilets/:uuid" $ do
    allowOrigin
    u <- param "uuid"
    let run = findOne $ select [uuid =: (u :: UUID)] toilets
    mDoc <- raiseET connect >>= fmap raiseET (access slaveOk run)
    if isNothing mDoc then status status404 else text "Oh Yeah"
  get "/toilets" $ do
    allowOrigin
    radius <- param "circle[radius]"
    center <- Location <$> param "circle[longitude]" <*> param "circle[latitude]"
    let run = fmap (fmap $ at uuid) $ find (inCircle radius center){project = [uuid =: (1 :: Int)]} >>= rest
    ids <- raiseET connect >>= fmap raiseET (access slaveOk run)
    json . (fmap toString) $ ids

allowOrigin :: ActionT Text (ReaderT Config IO) ()
allowOrigin = setHeader "Access-Control-Allow-Origin" "*"

raiseET :: (Show e, Monad m) => ErrorT e m a -> ActionT Text m a
raiseET = lift . runErrorT >=> either raiseShow return

raiseShow :: (Show s, Monad m) => s -> ActionT Text m a
raiseShow = raise . pack . show

instance Parsable UUID where
  parseParam = fmap (maybe (Left "parseParam UUID: no parse") return) $ fromString . unpack
