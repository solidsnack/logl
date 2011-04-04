{-# LANGUAGE StandaloneDeriving
           , GADTs
  #-}
module Language.LogL.Syntax where

import Data.ByteString.Char8
import Data.Time.Clock
import Data.Time.Format()
import Data.Tree
import Data.Vector

import Language.LogL.Tag (Tag)
import qualified Language.LogL.UUID as UUID


data LogL t where
  Alloc         ::  Log -> LogL (ID Log)
  Append        ::  ID Log -> ID Entry -> Message -> LogL (ID Entry)
  Free          ::  ID Log -> LogL ()
  Subtree       ::  ID Log -> ID Entry -> LogL (Tree Entry)
--Chain         ::  ID Log -> ID Entry -> ID Entry -> LogL (Vector Entry)

data Log                     =  Log !(ID Log) !UTCTime !Tag !UTCTime
deriving instance Eq Log
deriving instance Ord Log
deriving instance Show Log

data Entry                   =  Entry { log         :: !(ID Log),
                                        parent      :: !(ID Entry),
                                        uuid        :: !(ID Entry),
                                        timestamp   :: !UTCTime,
                                        tag         :: !Tag,
                                        client_time :: !UTCTime,
                                        bytes       :: !ByteString }
deriving instance Eq Entry
deriving instance Ord Entry
deriving instance Show Entry

data ID t                    =  ID !UUID.V1
deriving instance Eq (ID t)
deriving instance Ord (ID t)
deriving instance Show (ID t)

data Message                 =  Message !Tag !UTCTime !ByteString
deriving instance Eq Message
deriving instance Ord Message
deriving instance Show Message

