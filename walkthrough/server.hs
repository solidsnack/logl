#!/usr/bin/env runhaskell
{-# LANGUAGE ScopedTypeVariables
           , OverloadedStrings
           , NoMonomorphismRestriction
  #-}

import Control.Applicative
import Control.Exception as Exception
import System.Exit

import Network.Wai.Handler.Warp

import Language.LogL.Backend hiding (conninfo)
import Language.LogL.Server
import Language.LogL.PG


main                         =  do
  putStrLn "Starting backend..."
  bk :: Sharded Postgres    <-  catch
    (start ((2,3), conninfos))
    (errOut "An error occured while initializing the backend.")
  putStrLn "Starting server..."
  serve 0xFF bk defaultSettings Nothing
 where
  catch :: IO t -> (Exception.SomeException -> IO t) -> IO t
  catch                      =  Exception.catch
  errOut m e                 =  putStrLn m >> print e >> exitFailure

conninfos = conn <$> [ "logl0", "logl1", "logl2", "logl3", "logl4",
                       "logl5", "logl6", "logl7", "logl8", "logl9" ]
 where
  conn dbname                =  conninfo{ dbname=dbname, host="localhost",
                                          user="logl",   password="pwned" }

