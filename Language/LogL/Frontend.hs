{-# LANGUAGE GADTs
           , RecordWildCards
  #-}
module Language.LogL.Frontend where

import Control.Applicative
import Data.ByteString.Char8
import Data.Maybe
import Data.String
import Data.Time.Clock
import Data.Time.Format()
import Data.Tree
import Data.Vector

import Language.LogL.Tag (Tag)
import Language.LogL.UUID
import Language.LogL.Pickle
import Language.LogL.Syntax
import Language.LogL.Backend


data Frontend b where
  Frontend :: (Backend b) => {backend :: b} -> Frontend b


--  Remember, "pattern matching causes type refinement". Remember what that
--  actually means.
interpret                   ::  (Backend b) => Frontend b -> LogL t -> IO t 
interpret Frontend{..} logl  =  case logl of
  Alloc (client_time, tag)  ->  do
    uuid                    <-  ID <$> v1
    timestamp               <-  getCurrentTime
    let log                  =  Log uuid timestamp client_time tag
    Envelope t_ t' info st  <-  run backend (WriteLog log)
    undefined
  Append logID parentID msg ->  do
    let Message client_time tag bytes = msg
    uuid                    <-  ID <$> v1
    timestamp               <-  getCurrentTime
    let entry                =  Entry  uuid  logID        parentID  timestamp
                                             client_time  tag       bytes
    Envelope t_ t' info st  <-  run backend (WriteEntry entry)
    undefined
  Free logID                ->  do
    Envelope t_ t' info st  <-  run backend (WriteTombstone logID)
    undefined
  Subtree logID entryID     ->  do
    Envelope t_ t' info st  <-  run backend (RetrieveSubtree logID entryID)
    undefined

