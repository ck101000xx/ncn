module NCN.Scotty where
import Control.Applicative
import Control.Monad.Reader
import Network.Wai
import Network.Wai.Handler.Warp
import NCN.Config
import Web.Scotty.Trans

ncnScottyT :: ScottyT e (ReaderT Config IO) () -> ReaderT Config IO ()
ncnScottyT scotty = do
  (mHost, port) <- asks $ ((,) <$> serverHost <*> serverPort) . server
  let host = maybe (settingsHost defaultSettings) Host mHost
      options = Options 1 defaultSettings{settingsPort = port, settingsHost = host}
  cfg <- ask
  scottyOptsT options id (flip runReaderT cfg) scotty

ncnScottyAppT :: ScottyT e (ReaderT Config IO) () -> ReaderT Config IO Application
ncnScottyAppT scotty = do
  cfg <- ask
  scottyAppT id (flip runReaderT cfg) scotty
