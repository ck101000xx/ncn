module NCN.MongoDB where
import Control.Monad.Error
import Control.Monad.Reader
import Control.Monad.Morph
import Control.Monad.Trans
import Database.MongoDB as M
import NCN.Config

type ERC e m = ErrorT e (ReaderT Config m)

connect :: MonadIO m => ERC IOError m Pipe
connect = do
  host <- lift $ asks (mongoDBHost . mongodb)
  hoist liftIO $ M.connect host

access :: MonadIO m => AccessMode -> Action m a -> Pipe -> ERC Failure m a
access m a p = do
  db <- lift $ asks (mongoDBDatabase . mongodb)
  hoist lift . ErrorT $ M.access p m db a

