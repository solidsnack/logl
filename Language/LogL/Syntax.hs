{-# LANGUAGE OverloadedStrings
           , StandaloneDeriving
           , DisambiguateRecordFields
           , GADTs
  #-}
module Language.LogL.Syntax where

import Data.ByteString.Char8
import Data.Set (Set)
import Data.String
import Data.Time.Clock
import Data.Time.Format()

import Language.LogL.Tag (Tag)
import qualified Language.LogL.UUID as UUID


data LogL t where
  Alloc                     ::  Tag -> LogL (ID Log)
  Free                      ::  ID Log -> LogL ()
  Append                    ::  ID Log -> UTCTime -> Tag -> ByteString
                            ->  LogL (ID Entry)
  SearchEntries :: ID Log -> (UTCTime, UTCTime) -> ID Entry -> Word8 -> Tag
                -> LogL (Set (ID Entry))
  SearchLogs                ::  Set (ID Log) -> Tag -> LogL (Set (ID Log))


data Log                     =  Log !(ID Log) !UTCTime !Tag
deriving instance Eq Log
deriving instance Ord Log
deriving instance Show Log


data Entry                   =  Entry !(ID Entry) !(ID Log) !UTCTime
                                      !UTCTime !Tag !ByteString
deriving instance Eq Entry
deriving instance Ord Entry
deriving instance Show Entry

newtype ID t                 =  ID UUID.V1
deriving instance Eq (ID t)
deriving instance Ord (ID t)
deriving instance Show (ID t)

data Message                 =  Message !UTCTime !Tag !ByteString
deriving instance Eq Message
deriving instance Ord Message
deriving instance Show Message

