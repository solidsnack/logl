{-# LANGUAGE OverloadedStrings
           , EmptyDataDecls
           , StandaloneDeriving
           , GADTs
  #-}
module Language.LogL.Interpreter where

import Control.Applicative

import qualified Database.SQLite3 as SQLite3

import Language.LogL.Syntax


--  Responses should provide us enough information to correlate a user and
--  their actions.


data Response i t where
  OK          :: (Interpeter i) => !UTCTime -> Info i -> !t -> Response i t
  ERROR       :: (Interpeter i) => !UTCTime -> Info i -> Response i t
  TIMEOUT     :: (Interpeter i) => !UTCTime -> Info i -> Response i t


class Interpreter backend where
  Info backend              ::  *
  Spec backend              ::  *
  init                      ::  Spec backend -> IO backend
  shutdown                  ::  backend -> IO ()
  run :: forall t. backend -> LogL t -> IO (Result backend t)


data SQLite = SQLite { wait :: Word16, db :: SQLite3.Database,
                       path :: String, lock :: MVar ()         }
instance Backend SQLite where
  type Info SQLite           =  (String, SQLite3.Error)
  type Spec SQLite           =  (Word16, String)
  init wait path             =  do
    db                      <-  SQLite3.open s
    lock                    <-  newMVar ()
    return (SQLite wait path db lock)
  shutdown SQLite{..}        =  withMVar (SQLite3.close db)
  run SQLite{..} q           =  withMVar lock . forkIO . timeout info wait $ do
    undefined -- query the database


timeout :: (Interpreter i) ----------------------------------------------
        => Word16 -> Info i -> IO (Response i t) -> IO (Response i t)
timeout info w io            =  do
  start                     <-  getCurrentTime
  output                    <-  newEmptyMVar
  alarm                     <-  newEmptyMVar
  worker                    <-  forkIO $ do
                                  res    <- io
                                  ontime <- tryPutMvar output res
                                  when ontime (killThread =<< takeMVar alarm)
  (putMVar alarm =<<) . forkIO $ do
                          threadDelay (fromIntegral w)
                          res      <- TIMEOUT info start <$> getCurrentTime
                          overtime <- tryPutMVar output res
                          when overtime (killThread worker)
  takeMVar output

