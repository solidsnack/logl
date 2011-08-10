{-# LANGUAGE OverloadedStrings
           , StandaloneDeriving
           , NoMonomorphismRestriction
  #-}
import Database.PQ hiding (print)
import Data.Word
import Control.Monad
import System.IO.Error
import Control.Concurrent

import Language.LogL.PG


main                         =  simple =<< connectdb ""


simple conn                  =  do
  worker                    <-  forkIO $ do queryAsync conn
                                            putStrLn "completed"
  putStrLn "before"
  threadDelay 500000
  killThread worker
  putStrLn "after"
  print =<< isBusy conn


querySync conn               =  exec conn "SELECT pg_sleep(1000);"

queryAsync conn              =  do
  sendQuery conn "SELECT pg_sleep(1000);"
  polling
  getResult conn
 where
  polling = do
    threadDelay 25000
    stat <- consumeInput conn
    if stat then do busy <- isBusy conn
                    when busy polling
            else return ()

contend                      =  do
  lock                      <-  newMVar ()
  worker                    <-  forkIO . withMVar lock . const $ do
    threadDelay 10000000
    putStrLn "a"
  threadDelay 500000
  killThread worker
  withMVar lock . const $ putStrLn "b"

