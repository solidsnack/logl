{-| Datatypes and functions for request accounting. Each request may affect a
    log in up to four ways: allocating it, tombstoning it, appending entries
    and reading entries.
 -}
{-# LANGUAGE StandaloneDeriving
           , RecordWildCards
           , OverloadedStrings
  #-}
module Language.LogL.Accounting where

import Control.Applicative
import qualified Data.Map as Map
import Data.Map (Map)
import Data.Monoid

import qualified Data.Object as YAML
import qualified Data.Object.Yaml as YAML

import Language.LogL.Syntax
import Language.LogL.YAML (ToYAML(..))
import qualified Language.LogL.YAML as YAML
import qualified Language.LogL.Pickle as Pickle


{-| An account of resources touched in a particular log.
 -}
data Account = Account { tombstone :: Bool, alloc :: Bool, entries :: Entries }
instance Monoid Account where
  mempty                     =  Account False False mempty
  Account a b e `mappend` Account a' b' e' =
    Account (a || a') (b || b') (mappend e e')
instance ToYAML Account where
  oYAML Account{..}          =  YAML.Mapping
    $  if tombstone  then [(YAML.s "tombstone", YAML.sy "true")] else []
    ++ if alloc      then [(YAML.s "alloc",     YAML.sy "true")] else []
    ++ if anyEntries then [(YAML.s "entries",   entryInfo     )] else []
   where
    entryInfo                =  oYAML entries
    anyEntries               =  case entryInfo of YAML.Mapping (_:_) -> True
                                                  _                  -> False

{-| Account of affected log entries -- read or written.
 -}
data Entries = Entries { read :: [ID Entry], append :: [ID Entry] }
instance Monoid Entries where
  mempty                     =  Entries [] []
  Entries r a `mappend` Entries r' a' = Entries (mappend r r') (mappend a a')
instance ToYAML Entries where
  oYAML Entries{..}          =  YAML.Mapping
    $  if read /= []   then [(YAML.s "read",      fromList read  )] else []
    ++ if append /= [] then [(YAML.s "append",    fromList append)] else []
   where
    fromList                 =  YAML.Sequence . ((YAML.sy . Pickle.o) <$>)

{-| A collection of accounts across many logs. A 'Monoid' instance provides
    for merging of accounts.
 -}
newtype Accounts             =  Accounts (Map (ID Log) Account)
instance Monoid Accounts where
  mempty                     =  Accounts Map.empty
  Accounts m `mappend` Accounts m' = Accounts (Map.unionWith mappend m m')
instance ToYAML Accounts where
  oYAML (Accounts m)         =  YAML.Mapping (yamlKV <$> Map.toAscList m)
   where
    yamlKV (k, v)            =  (YAML.s (Pickle.o k), oYAML v)

{-| Merge an account with a collection of accounts.
 -}
add                         ::  ID Log -> Account -> Accounts -> Accounts
add logID account accounts =
  accounts `mappend` Accounts (Map.fromList [(logID, account)])

