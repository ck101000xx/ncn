module NCN.MongoDB where
import Control.Monad.Reader
import Database.MongoDB as M
import NCN.Config

type RC = ReaderT MongoDBConfig
connect :: RC IOE Pipe
connect = asks mongoDBHost >>= lift . M.connect

access :: AccessMode -> Action m a -> Pipe -> RC m (Either Failure a)
access m a p = asks mongoDBDatabase >>= lift . ($a) . M.access p m
