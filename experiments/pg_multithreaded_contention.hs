{-# LANGUAGE OverloadedStrings
           , StandaloneDeriving
           , NoMonomorphismRestriction
  #-}
{- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

  Connect to PG. Spawn two threads:

 .  Long running thread that hogs connection.

 .  Thread to quickly kill the first thread.

  Preliminary results: If if you interrupt the thread, you will get a result
  right away but any further actions on the connection will block.

 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -}

import Database.PQ
import Data.Word
import Control.Monad
import System.IO.Error
import Control.Concurrent


--main                         =  contend
--main                         =  interrupt =<< connectdb ""
main                         =  lessfancy =<< connectdb ""


interrupt conn               =  do
--Just cancelToken          <-  getCancel conn
  val                       <-  timeout 5000 (queryDoesNotBlockKill conn)
  putStrLn "Hello."
--showLn val
--showLn =<< cancel cancelToken
  showLn =<< isBusy conn
--showLn =<< transactionStatus conn
--showLn =<< connectPoll conn
--showLn =<< status conn


queryDoesNotBlockKill conn   =  exec conn "SELECT pg_sleep(10);"

queryBlocksKill conn         =  do
  sendQuery conn "SELECT pg_sleep(10);"
  getResult conn


timeout                     ::  Word32 -> IO t -> IO (Maybe t)
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

deriving instance Show TransactionStatus


contend                      =  do
  lock                      <-  newMVar ()
  worker                    <-  forkIO . withMVar lock . const $ do
    threadDelay 10000000
    putStrLn "a"
  threadDelay 500000
  killThread worker
  withMVar lock . const $ putStrLn "b"


lessfancy conn               =  do
  worker                    <-  forkIO $ do queryDoesNotBlockKill conn
                                            putStrLn "completed"
  putStrLn "before"
  threadDelay 500000
  killThread worker
  putStrLn "after"
  showLn =<< isBusy conn


showLn                       =  putStrLn . show

