{-# LANGUAGE RecordWildCards
           , GADTs
  #-}
module Language.LogL.Interpret where

import Control.Applicative
import qualified Data.Graph as Graph
import Data.Time.Clock
import Data.Time.Format()
import Data.Tree

import Language.LogL.UUID
import Language.LogL.Syntax
import Language.LogL.Backend


{-| Interpret a LogL request with the aid of a backend.
 -}
interpret                   ::  (Backend b) => b -> LogL t -> IO (Status t)
interpret backend logl       =  case logl of
  Alloc client_time tag     ->  do
    uuid                    <-  ID <$> v1
    timestamp               <-  getCurrentTime
    let log                  =  Log uuid timestamp client_time tag
    WriteLog log `pipe` const uuid
  Append logID parentID ms  ->  do
      trees                 <-  mapM (appendTree logID parentID) ms
      return $ if any (== ERROR) trees then ERROR
                                       else OK (statusListToList trees)
  Free logID                ->  WriteTombstone logID `pipe` const ()
  Forest logID entryID      ->  RetrieveForest logID entryID `pipe` forest
 where
  run' task                  =  do
    Envelope _ _ _ status   <-  run backend task
    return status
  pipe task f                =  (f <$>) <$> run' task
  appendTree logID parentID Node{..} = do
    uuid                    <-  ID <$> v1
    timestamp               <-  getCurrentTime
    let Message client_time tag bytes = rootLabel
        entry                =  Entry  uuid  logID        parentID  timestamp
                                             client_time  tag       bytes
    stat                    <-  WriteEntry entry `pipe` const uuid
    case stat of
      OK _                  ->  do
        trees               <-  mapM (appendTree logID uuid) subForest
        return $ if any (== ERROR) trees
                   then  ERROR
                   else  OK Node{ rootLabel=uuid,
                                  subForest=(statusListToList trees) }
      ERROR                 ->  return ERROR


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

