module NCN.MongoDB where
import Database.MongoDB
class Entity a where
  toDocument :: a -> Document
  fromDocument :: Document -> Maybe a

