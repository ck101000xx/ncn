module NCN.MongoDB where
import Control.Applicative
import Control.Monad
import Control.Monad.Error
import Control.Monad.Reader
import Control.Monad.Morph
import Control.Monad.Trans
import Data.Maybe
import Database.MongoDB as M
import NCN.Config

type ERC e m = ErrorT e (ReaderT Config m)

connect :: MonadIO m => ERC IOError m Pipe
connect = do
  host <- lift $ asks (mongoDBHost . mongodb)
  hoist liftIO $ M.connect host

access :: (MonadIO m, Applicative m, Functor m) => AccessMode -> Action m a -> Pipe -> ERC Failure m a
access mode action pipe = do
  (db, auth') <- lift . asks $ (liftM2 (,) mongoDBDatabase authAction) . mongodb
  hoist lift . ErrorT $ M.access pipe mode db (auth' >> action)
  where authAction = fromMaybe (return True) . liftM2 (liftM2 auth) mongoDBUsername mongoDBPassword

