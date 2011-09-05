{-| Datatypes and functions for request accounting. Each request may affect a
    log in up to four ways: allocating it, tombstoning it, appending entries
    and reading entries.
 -}
{-# LANGUAGE StandaloneDeriving
  #-}
module Language.LogL.Accounting where

import qualified Data.Map as Map
import Data.Map (Map)
import Data.Monoid

import Language.LogL.Syntax


{-| An account of resources touched in a particular log.
 -}
data Account = Account { tombstone :: Bool, alloc :: Bool, entries :: Entries }
instance Monoid Account where
  mempty                     =  Account False False mempty
  Account a b e `mappend` Account a' b' e' = Account (a || a') (b || b')
                                                     (mappend e e')

{-| Account of affected log entries -- read or written.
 -}
data Entries = Entries { read :: [ID Entry], append :: [ID Entry] }
instance Monoid Entries where
  mempty                     =  Entries [] []
  Entries r a `mappend` Entries r' a' = Entries (mappend r r') (mappend a a')

{-| A collection of accounts across many logs. A 'Monoid' instance provides
    for merging of accounts.
 -}
newtype Accounts             =  Accounts (Map (ID Log) Account)
instance Monoid Accounts where
  mempty                     =  Accounts Map.empty
  Accounts m `mappend` Accounts m' = Accounts (Map.unionWith mappend m m')

{-| Merge an account with a collection of accounts.
 -}
add                         ::  ID Log -> Account -> Accounts -> Accounts
add logID account accounts =
  accounts `mappend` Accounts (Map.fromList [(logID, account)])

