module Language.LogL.Interpret where

import Control.Applicative
import qualified Data.Graph as Graph
import Data.Monoid
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
  Forest logID entryID      ->  RetrieveForest logID entryID `pipe` forest
 where
  run'                      ::  (Monoid t) => Task t -> IO (Status t)
  run' task                  =  do
    Envelope _ _ _ status   <-  run backend task
    return status
  pipe task f                =  (f <$>) <$> run' task


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

