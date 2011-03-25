{-# LANGUAGE OverloadedStrings
           , EmptyDataDecls
           , StandaloneDeriving
           , GADTs
           , RecordWildCards
           , TypeSynonymInstances
  #-}
module Language.LogL.Interpreter where

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

import qualified Language.LogL.Pickle as Pickle
import Language.LogL.Syntax


data Envelope i t where
  Envelope :: (Interpreter i) ---------------------------------------------
           => !UTCTime -> !UTCTime -> Info i -> Result t -> Envelope i t

data Result t                =  OK !t | ERROR

{-| Backends support a limited language -- write info in log slot, write info
    in message slot, retrieve log info and retrieve message.
 -}
data BackendL t where
  SetLog                    ::  ID Log -> UTCTime -> BackendL ()
  SetEntry                  ::  ID Log -> ID Entry -> UTCTime -> BackendL ()
  GetLog                    ::  ID Log -> BackendL Log
  GetEntry                  ::  ID Log -> ID Entry -> BackendL Entry
  ClearLog                  ::  ID Log -> BackendL ()
  ClearEntry                ::  ID Log -> ID Entry -> BackendL ()


class Interpreter backend where
  type Info backend         ::  *
  type Spec backend         ::  *
  start                     ::  Spec backend -> IO backend
  stop                      ::  backend -> IO ()
  run :: forall t. backend -> BackendL t -> IO (Envelope backend t)


data SQLite = SQLite { db :: SQLite3.Database, table :: String,
                       path :: String, lock :: MVar ()          }
instance Interpreter SQLite where
  type Info SQLite           =  ByteString
  type Spec SQLite           =  (String, String)
  start (table, path)        =  do
    db                      <-  SQLite3.open path
    stmt                    <-  SQLite3.prepare db (sqlite_table_stmt table)
    _                       <-  SQLite3.step stmt
    SQLite3.finalize stmt
    SQLite db table path <$> newMVar ()
  stop SQLite{..}            =  withMVar' lock (SQLite3.close db)
  run SQLite{..} q           =  withMVar' lock . envelope $ do
    result                  <-  tryJust check_exc $ do
                                  stmt <- SQLite3.prepare db sql
                                  return ERROR
    return $ case result of
      Left msg              ->  (Pickle.o msg, ERROR)
      Right val             ->  ("", val)
   where
    sql                      =  undefined
    check_exc exc = (guard . isUserError) exc >> Just (ioeGetErrorString exc)

sqlite_table_stmt name = unlines [ "CREATE TABLE IF NOT EXISTS " ++ name
                                 , "  ( " ++ joined_schema ++ " );"
                                 , "CREATE INDEX IF NOT EXISTS ON " ++ name
                                 , "  ( user_timestamp );"
                                 , "CREATE INDEX IF NOT EXISTS ON " ++ name
                                 , "  ( timestamp );" ]
 where
  joined_schema              =  intercalate "\n  , " sqlite_schema

sqlite_schema                =  [ "uuid           CHAR(36) PRIMARY KEY"
                                , "user_timestamp VARCHAR(40) NOT NULL"
                                , "timestamp      VARCHAR(40) NOT NULL"
                                , "data           BLOB NOT NULL" ]


data Timeout i               =  Timeout { wait :: Word16, worker :: i }
data TimeoutInfo i           =  TIMEOUT | (Interpreter i) => COMPLETED (Info i)
instance (Interpreter i) => Interpreter (Timeout i) where
  type Info (Timeout i)      =  TimeoutInfo i
  type Spec (Timeout i)      =  (Word16, Spec i)
  start (wait, spec)         =  Timeout wait <$> start spec
  stop Timeout{..}           =  stop worker
  run Timeout{..} q          =  envelope $ do
    res                     <-  timeout wait (run worker q)
    return $ case res of
      Just (Envelope _ _ info val) -> (COMPLETED info, val)
      Nothing               ->  (TIMEOUT, ERROR)

timeout                     ::  Word16 -> IO t -> IO (Maybe t)
timeout w io                 =  do
  output                    <-  newEmptyMVar
  alarm                     <-  newEmptyMVar
  worker                    <-  forkIO $ do
                                  res    <- io
                                  ontime <- tryPutMVar output (Just res)
                                  when ontime (killThread =<< takeMVar alarm)
  (putMVar alarm =<<) . forkIO $ do
                          threadDelay (fromIntegral w)
                          overtime <- tryPutMVar output Nothing
                          when overtime (killThread worker)
  takeMVar output


withMVar'                   ::  MVar t' -> IO t -> IO t
withMVar' mvar               =  withMVar mvar . const


envelope :: (Interpreter i) => IO (Info i, Result t) -> IO (Envelope i t)
envelope io                  =  do
    start                   <-  getCurrentTime
    (msg, val)              <-  io
    stop                    <-  getCurrentTime
    return $ Envelope start stop msg val

