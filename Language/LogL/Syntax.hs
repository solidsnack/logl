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
import Data.Vector

import Language.LogL.Tag (Tag)
import qualified Language.LogL.UUID as UUID
import Language.LogL.Pickle


data LogL t where
  Alloc                     ::  Log -> LogL (ID Log)
  Append        ::  ID Log -> ID Entry -> Message -> LogL (ID Entry)
  Free                      ::  ID Log -> LogL ()
  Subtree                   ::  ID Log -> ID Entry -> LogL (Tree Entry)
--Chain         ::  ID Log -> ID Entry -> ID Entry -> LogL (Vector Entry)

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

data ID t                    =  ID !UUID.V1
deriving instance Eq (ID t)
deriving instance Ord (ID t)
deriving instance Show (ID t)
instance Pickle (ID t) where
  i                          =  fmap ID . i
  o (ID v1)                  =  o v1
instance IsString (ID t) where
  fromString                 =  fromJust . i . pack

data Message                 =  Message !UTCTime !Tag !ByteString
deriving instance Eq Message
deriving instance Ord Message
deriving instance Show Message

