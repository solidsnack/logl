{-| Datatypes and functions for request accounting. Each request may affect a
    log in up to four ways: allocating it, tombstoning it, appending entries
    and reading entries.
 -}
module Language.LogL.Accounting where

import Data.Map

data Account = Account { tombstone :: Bool, alloc :: Bool, entries :: Entries }
instance Monoid Account where
  mempty                     =  Account False False mempty
  mappend (Account a b e) (Account a' b' e') = Account (a || a')      (b || b')
                                                       (mappend e e')

data Entries = Entries { read :: [ID Entry], append :: [ID Entry] }
instance Monoid Entries where
  mempty                     =  Entries [] []
  mappend (Entries r a) (Entries r' a') = Entries (mappend r r') (mappend a a')

