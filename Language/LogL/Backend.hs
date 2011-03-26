{-# LANGUAGE OverloadedStrings
           , StandaloneDeriving
           , GADTs
           , RecordWildCards
           , TypeSynonymInstances
           , TemplateHaskell
  #-}
module Language.LogL.Backend where

import Control.Applicative
import Control.Concurrent
import Control.Exception
import Control.Monad
import Data.ByteString.Char8 (ByteString)
import Data.List
import Data.Time.Clock
import Data.Word
import System.IO.Error

import qualified Database.SQLite3 as SQLite3

import Language.LogL.Syntax
import qualified Language.LogL.Pickle as Pickle
import qualified Language.LogL.Macros as Macros


data Envelope i where
  Envelope :: (Backend i) ---------------------------------------------
           => !UTCTime -> !UTCTime -> Info i -> Result -> Envelope i

--  TODO: Put 'legit output' constraint on OK constructor.
data Result                  =  forall t. OK !t | ERROR

{-| Backends support a few tasks, allowing us to set, delete and retrieve logs
    and log entries. Backends are completely permissive as regards
    over-writing existing entries and so forth -- single-assignment invariants
    come from the interpreter layer, which manages the IDs.
 -}
data Task                    =  SetLog !(ID Log) !UTCTime
                             |  SetEntry !(ID Log) !(ID Entry) !UTCTime
                             |  GetLog !(ID Log)
                             |  GetEntry !(ID Log) !(ID Entry)
                             |  DeleteLog !(ID Log)
                             |  DeleteEntry !(ID Log) !(ID Entry)
                             |  EntryTimeRangeSearch !(ID Log)
                             |  LogTagSearch !ByteString128 [ID Log]


class Backend backend where
  type Info backend         ::  *
  type Spec backend         ::  *
  start                     ::  Spec backend -> IO backend
  stop                      ::  backend -> IO ()
  run                       ::  backend -> [Task] -> IO [Envelope backend]


data SQLite
  = SQLite { db :: SQLite3.Database, path :: String, lock :: MVar () }
instance Backend SQLite where
  type Info SQLite           =  ByteString
  type Spec SQLite           =  String
  start path                 =  do
    db                      <-  SQLite3.open path
    prep_step_fin db logs
    prep_step_fin db entries
    SQLite db path <$> newMVar ()
   where
    (logs, entries)          =  $(Macros.text_blocks "./sqlite/ddl.sql")
    prep_step_fin db str     =  do
      stmt                  <-  SQLite3.prepare db str
      _                     <-  SQLite3.step stmt
      SQLite3.finalize stmt
  stop SQLite{..}            =  withMVar' lock (SQLite3.close db)
  run SQLite{..} queries     =  withMVar' lock . sequence
                             $  (envelope . run_once) <$> queries
   where
    check_exc exc = (guard . isUserError) exc >> Just (ioeGetErrorString exc)
    run_once q               =  do
      result                <-  tryJust check_exc $ do
                                    stmt <- SQLite3.prepare db ""
                                    return ERROR
      return $ case result of
        Left msg            ->  (Pickle.o msg, ERROR)
        Right val           ->  ("", val)


--data Timeout i               =  Timeout { wait :: Word16, worker :: i }
--data TimeoutInfo i           =  TIMEOUT | (Backend i) => COMPLETED (Info i)
--instance (Backend i) => Backend (Timeout i) where
--  type Info (Timeout i)      =  TimeoutInfo i
--  type Spec (Timeout i)      =  (Word16, Spec i)
--  start (wait, spec)         =  Timeout wait <$> start spec
--  stop Timeout{..}           =  stop worker
--  run Timeout{..} q          =  envelope $ do
--    res                     <-  timeout wait (run worker q)
--    return $ case res of
--      Just (Envelope _ _ info val) -> (COMPLETED info, val)
--      Nothing               ->  (TIMEOUT, ERROR)

--timeout                     ::  Word16 -> IO t -> IO (Maybe t)
--timeout w io                 =  do
--  output                    <-  newEmptyMVar
--  alarm                     <-  newEmptyMVar
--  worker                    <-  forkIO $ do
--                                  res    <- io
--                                  ontime <- tryPutMVar output (Just res)
--                                  when ontime (killThread =<< takeMVar alarm)
--  (putMVar alarm =<<) . forkIO $ do
--                          threadDelay (fromIntegral w)
--                          overtime <- tryPutMVar output Nothing
--                          when overtime (killThread worker)
--  takeMVar output


withMVar'                   ::  MVar t' -> IO t -> IO t
withMVar' mvar               =  withMVar mvar . const


envelope :: (Backend i) => IO (Info i, Result) -> IO (Envelope i)
envelope io                  =  do
    start                   <-  getCurrentTime
    (msg, val)              <-  io
    stop                    <-  getCurrentTime
    return $ Envelope start stop msg val

