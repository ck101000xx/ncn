module NCN.Acid where

import Data.IxSet as I
import Data.Acid
import NCN.Config.Acid as Config
type Database = String

open :: Database -> ReaderT DatabaseConfig IO (AcidState IxSet a)
open db = do
  path <- fmap Config.path  ask
  openLocalStateFrom  (path ++ db)  I.empty

users = "users"
tokens = "tokens"