{-# LANGUAGE OverloadedStrings
           , StandaloneDeriving
           , DisambiguateRecordFields
           , GADTs
  #-}
module Language.LogL.Syntax where

import Data.ByteString.Char8
import Data.Time.Clock
import Data.Time.Format()

import Language.LogL.Tag (Tag)
import qualified Language.LogL.UUID as UUID


data LogL t where
  Post                      ::  !ID -> !Message -> LogL ID
  Free                      ::  !ID -> LogL ()
  Leaves                    ::  !ID -> LogL [Entry]
  Chain                     ::  !ID -> !ID -> LogL [Entry]

data Entry                   =  Entry !ID !ID !UTCTime !Tag !UTCTime
                                      !ByteString
deriving instance Eq Entry
deriving instance Ord Entry
deriving instance Show Entry

newtype ID                   =  ID UUID.V1
deriving instance Eq ID
deriving instance Ord ID
deriving instance Show ID

data Message                 =  Message !Tag !UTCTime !ByteString
deriving instance Eq Message
deriving instance Ord Message
deriving instance Show Message

