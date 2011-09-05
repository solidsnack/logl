{-# LANGUAGE RecordWildCards
           , ScopedTypeVariables
           , TupleSections
           , GADTs
  #-}
module Language.LogL.Interpret where

import Control.Applicative
import Control.Concurrent
import qualified Data.Graph as Graph
import Data.Monoid
import Data.Time.Clock
import Data.Time.Format()
import qualified Data.Traversable
import Data.Tree

import Language.LogL.Accounting (Accounts)
import qualified Language.LogL.Accounting as Accounting
import Language.LogL.Backend
import Language.LogL.Syntax
import Language.LogL.UUID


{-| Interpret a LogL request with the aid of a backend.
 -}
interpret :: (Backend b) => b -> LogL t -> IO (Status t, Accounts)
-- interpret                   ::  (Backend b) => b -> LogL t -> IO (Status t)
interpret backend logl       =  do
  accounts                  <-  newMVar mempty
  let mergeAccount = ((modifyMVar_ accounts . (return .)) .) . Accounting.add
  case logl of
    Alloc client_time tag   ->  do
      uuid                  <-  ID <$> v1
      mergeAccount uuid mempty{Accounting.alloc=True}
      timestamp             <-  getCurrentTime
      let log                =  Log uuid timestamp client_time tag
      status                <-  WriteLog log `pipe` const uuid
      (status,) <$> readMVar accounts
    Append logID parentID ms -> do
      asFullEntryTrees      <-  mapM (messagesToEntries logID parentID) ms
      let uuids = ((uuid <$>) . concat) (flatten <$> asFullEntryTrees)
      mergeAccount logID (appendAcc uuids)
      results               <-  mapM appendEntryTree asFullEntryTrees
      let status | any (== ERROR) results = ERROR
                 | otherwise              = OK (statusListToList results)
      (status,) <$> readMVar accounts
    Free logID              ->  do
      mergeAccount logID mempty{Accounting.tombstone=True}
      status                <-  WriteTombstone logID `pipe` const ()
      (status,) <$> readMVar accounts
    Forest logID entryID    ->  do
      status                <-  run' (RetrieveForest logID entryID)
      case status of
        ERROR               ->  return ()
        OK list             ->  mergeAccount logID (readAcc (uuid <$> list))
      (forest <$> status,) <$> readMVar accounts
 where
  readAcc list = mempty{Accounting.entries=mempty{Accounting.read=list}}
  appendAcc list = mempty{Accounting.entries=mempty{Accounting.append=list}}
  run' task                  =  do
    Envelope _ _ _ status   <-  run backend task
    return status
  pipe task f                =  (f <$>) <$> run' task
  appendEntryTree Node{rootLabel=Entry{..}, ..} = do
    stat                    <-  WriteEntry Entry{..} `pipe` const uuid
    case stat of
      OK _                  ->  do
        subResults          <-  mapM appendEntryTree subForest
        return $ if any (== ERROR) subResults
                   then  ERROR
                   else  OK Node{ rootLabel=uuid,
                                  subForest=(statusListToList subResults) }
      ERROR                 ->  return ERROR
  messagesToEntries logID parentID Node{..} = do
    uuid                    <-  ID <$> v1
    timestamp               <-  getCurrentTime
    trees                   <-  mapM (messagesToEntries logID uuid) subForest
    let Message client_time tag bytes = rootLabel
        entry = Entry uuid logID parentID timestamp client_time tag bytes
    return Node{rootLabel=entry, subForest=trees}


{-| Use the backedges in the entries to build up history trees.
 -}
forest                      ::  [Entry] -> [Tree Entry]
forest entries               =  (_1of3 . lookup <$>) <$> forest
 where
  (graph, lookup, _)         =  Graph.graphFromEdges (adjacency <$> entries)
  forest                     =  Graph.dff (Graph.transposeG graph)
  _1of3 (x, _, _)            =  x


{-| Convenience function to convert an 'Entry' into a graph element.
 -}
adjacency                   ::  Entry -> (Entry, ID Entry, [ID Entry])
adjacency entry@Entry{..}    =  (entry, uuid, [parent])

