module NCN.Config.Openshift where
import Control.Applicative
import Data.Text
import Database.MongoDB
import NCN.Config
import System.Environment

getConfig :: IO Config
getConfig = Config <$> getServerConfig <*> getMongoDBConfig
  where
    getServerConfig =
      ServerConfig <$>
        lookupEnv "OPENSHIFT_HASKELL_IP" <*>
        (read <$> getEnv "OPENSHIFT_HASKELL_PORT")
    getMongoDBConfig =
      MongoDBConfig <$>
        getHost <*>
        (pack <$> getEnv "OPENSHIFT_APP_NAME") <*>
        (fmap pack <$> lookupEnv "OPENSHIFT_MONGODB_DB_USERNAME") <*>
        (fmap pack <$> lookupEnv "OPENSHIFT_MONGODB_DB_PASSWORD")
    getHost =
      Host <$>
        getEnv "OPENSHIFT_MONGODB_DB_HOST" <*>
        (PortNumber . fromIntegral . read <$> getEnv "OPENSHIFT_MONGODB_DB_PORT")
