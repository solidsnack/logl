{-# LANGUAGE GADTs
           , RecordWildCards
  #-}
module Language.LogL.Frontend where

import Control.Applicative
import Data.ByteString.Char8
import Data.Maybe
import Data.Monoid
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
interpret :: (Backend b) => Frontend b -> LogL t -> IO (Status t)
interpret Frontend{..} logl  =  case logl of
  Alloc (client_time, tag)  ->  do
    uuid                    <-  ID <$> v1
    timestamp               <-  getCurrentTime
    let log                  =  Log uuid timestamp client_time tag
    runGuarded (WriteLog log) (const uuid)
  Append logID parentID msg ->  do
    uuid                    <-  ID <$> v1
    timestamp               <-  getCurrentTime
    let Message client_time tag bytes = msg
        entry                =  Entry  uuid  logID        parentID  timestamp
                                             client_time  tag       bytes
    runGuarded (WriteEntry entry) (const uuid)
  Free logID                ->  runGuarded (WriteTombstone logID) (const ())
  Forest logID entryID      ->  do
    runGuarded (RetrieveForest logID entryID) (const [])
 where
  runGuarded :: (Monoid t) => Task t -> (t -> t') -> IO (Status t')
  runGuarded task transform  =  do
    Envelope _ _ _ status   <-  run backend task
    case status of OK t     ->  return . OK . transform $ t
                   ERROR    ->  return ERROR

