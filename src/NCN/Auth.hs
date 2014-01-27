{-# LANGUAGE TemplateHaskell, TypeFamilies, FlexibleInstances, DeriveDataTypeable #-}
module NCN.Auth where

import Control.Monad.Reader.Class
import Control.Monad.State.Class
import Data.Acid
import Data.Either
import Data.Hashable
import Data.HashMap.Strict as M
import Data.SafeCopy
import Data.UUID
import Data.UUID.V4
import NCN.Types

type Token = UUID
deriveSafeCopy 1 'base ''UUID
type TokenMap = HashMap Token Email

instance (Hashable a, Eq a, SafeCopy a, SafeCopy b) => SafeCopy (HashMap a b) where
  getCopy = contain $ fmap fromList safeGet
  putCopy = contain . safePut . toList
  version = 1
  kind = base

insertToken :: Email -> Token -> Update TokenMap ()
insertToken email token = get >>= put . insert token email

newToken :: IO Token
newToken = nextRandom

deleteToken :: Token -> Update TokenMap ()
deleteToken token = get >>= put . delete token

getEmail :: Token -> Query TokenMap (Maybe Email)
getEmail token = fmap (M.lookup token) ask

makeAcidic ''TokenMap ['insertToken, 'deleteToken, 'getEmail]
