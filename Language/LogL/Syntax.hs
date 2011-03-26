{-# LANGUAGE OverloadedStrings
           , EmptyDataDecls
           , TypeFamilies
           , StandaloneDeriving
           , GADTs
  #-}
module Language.LogL.Syntax where

import Data.ByteString.Char8
import Data.String
import Data.Time.Clock
import Data.Time.Format()

import qualified Language.LogL.UUID as UUID


data LogL t where
  Alloc                     ::  ByteString128 -> LogL (ID Log)
  Free                      ::  ID Log -> LogL ()
  Append                    ::  ID Log -> Message -> LogL (ID Entry)
  ClipBefore                ::  ID Entry -> LogL ()


data Log                     =  Log (Meta Log)
deriving instance Eq Log
deriving instance Ord Log
deriving instance Show Log

data Entry                   =  Entry (Meta Entry)
deriving instance Eq Entry
deriving instance Ord Entry
deriving instance Show Entry

newtype ID t                 =  ID UUID.V1
deriving instance Eq (ID t)
deriving instance Ord (ID t)
deriving instance Show (ID t)

data Message                 =  Message { stamp :: !UTCTime,
                                          body :: !ByteString } 
deriving instance Eq Message
deriving instance Ord Message
deriving instance Show Message

newtype ByteString128        =  ByteString128 ByteString
deriving instance Eq ByteString128
deriving instance Ord ByteString128
deriving instance Show ByteString128
deriving instance IsString ByteString128


type Meta t                  =  ((ID t), UTCTime)

