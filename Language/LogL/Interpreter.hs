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
import Control.Concurrent.MVar
import Control.Exception
import Control.Monad
import Data.ByteString.Char8 (ByteString)
import Data.Time.Clock
import Data.Word
import System.IO.Error

import qualified Database.SQLite3 as SQLite3

import qualified Language.LogL.Pickle as Pickle
import Language.LogL.Syntax


data Envelope i t where
  Envelope :: (Interpreter i) ---------------------------------------------
           => !UTCTime -> !UTCTime -> Info i -> Result t -> Envelope i t

data Result t                =  OK !t | ERROR | TIMEOUT


class Interpreter backend where
  type Info backend         ::  *
  type Spec backend         ::  *
  init                      ::  Spec backend -> IO backend
  shutdown                  ::  backend -> IO ()
  run :: forall t. backend -> LogL t -> IO (Envelope backend t)


data SQLite = SQLite { wait :: Word16, db :: SQLite3.Database,
                       path :: String, lock :: MVar ()         }
instance Interpreter SQLite where
  type Info SQLite           =  ByteString
  type Spec SQLite           =  (Word16, String)
  init (wait, path)          =  do
    db                      <-  SQLite3.open path
    lock                    <-  newMVar ()
    return (SQLite wait db path lock)
  shutdown SQLite{..}        =  withMVar' lock (SQLite3.close db)
  run SQLite{..} q           =  withMVar' lock $ do
    start                   <-  getCurrentTime
    result                  <-  tryJust check_exc . timeout wait $ do
                                  SQLite3.prepare db sql
                                  return ERROR
    let (msg, val)           =  case result of
                                  Left msg  -> (Pickle.o msg, ERROR)
                                  Right val -> ("", val)
    stop                    <-  getCurrentTime
    return $ Envelope start stop msg val
   where
    sql                      =  undefined
    check_exc exc = (guard . isUserError) exc >> Just (ioeGetErrorString exc)


timeout                     ::  Word16 -> IO (Result t) -> IO (Result t)
timeout w io                 =  do
  output                    <-  newEmptyMVar
  alarm                     <-  newEmptyMVar
  worker                    <-  forkIO $ do
                                  res    <- io
                                  ontime <- tryPutMVar output res
                                  when ontime (killThread =<< takeMVar alarm)
  (putMVar alarm =<<) . forkIO $ do
                          threadDelay (fromIntegral w)
                          overtime <- tryPutMVar output TIMEOUT
                          when overtime (killThread worker)
  takeMVar output


withMVar'                   ::  MVar t' -> IO t -> IO t
withMVar' mvar               =  withMVar mvar . const
