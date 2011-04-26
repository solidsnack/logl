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
    WriteLog log `pipe` const uuid
  Append logID parentID msg ->  do
    uuid                    <-  ID <$> v1
    timestamp               <-  getCurrentTime
    let Message client_time tag bytes = msg
        entry                =  Entry  uuid  logID        parentID  timestamp
                                             client_time  tag       bytes
    WriteEntry entry `pipe` const uuid
  Free logID                ->  WriteTombstone logID `pipe` const ()
  Forest logID entryID      ->  RetrieveForest logID entryID `pipe` forests
 where
  run'                      ::  (Monoid t) => Task t -> IO (Status t)
  run' task                  =  do
    Envelope _ _ _ status   <-  run backend task
    return status
  pipe task f               =  (f <$>) <$> run' task


forests                     ::  [Entry] -> [Tree Entry]
forests                      =  const []

