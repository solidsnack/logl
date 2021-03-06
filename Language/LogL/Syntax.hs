{-# LANGUAGE StandaloneDeriving
           , GADTs
  #-}
module Language.LogL.Syntax where

import Data.ByteString.Char8
import Data.Maybe
import Data.String
import Data.Time.Clock
import Data.Time.Format()
import Data.Tree

import Language.LogL.Tag (Tag)
import Language.LogL.UUID
import Language.LogL.Pickle


data LogL t where
  Alloc             ::  UTCTime -> Tag -> LogL (ID Log)
  Append :: ID Log -> ID Entry -> [Tree Message] -> LogL [Tree (ID Entry)]
  Free              ::  ID Log -> LogL ()
  Forest            ::  ID Log -> ID Entry -> LogL [Tree Entry]
deriving instance Eq (LogL t)
--deriving instance Ord (LogL t) -- Leads to: error "Urk! in TcGenDeriv"
deriving instance Show (LogL t)

data Log                     =  Log !(ID Log) !UTCTime !UTCTime !Tag
deriving instance Eq Log
deriving instance Ord Log
deriving instance Show Log

data Entry                   =  Entry { uuid        :: !(ID Entry),
                                        log         :: !(ID Log),
                                        parent      :: !(ID Entry),
                                        timestamp   :: !UTCTime,
                                        client_time :: !UTCTime,
                                        tag         :: !Tag,
                                        bytes       :: !ByteString }
deriving instance Eq Entry
deriving instance Ord Entry
deriving instance Show Entry

data ID t                    =  ID !UUID
deriving instance Eq (ID t)
deriving instance Ord (ID t)
deriving instance Show (ID t)
instance Pickle (ID t) where
  i                          =  fmap ID . i
  o (ID uuid)                =  o uuid
instance IsString (ID t) where
  fromString                 =  fromJust . i . pack

data Message                 =  Message !UTCTime !Tag !ByteString
deriving instance Eq Message
deriving instance Ord Message
deriving instance Show Message

