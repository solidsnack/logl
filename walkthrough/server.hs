#!/usr/bin/env runhaskell
{-# LANGUAGE ScopedTypeVariables
           , OverloadedStrings
  #-}

import Control.Applicative

import Network.Wai.Handler.Warp

import Language.LogL.Backend hiding (conninfo)
import Language.LogL.Server
import Language.LogL.PG
--import qualified Language.LogL.PG as PG


main                         =  do
  putStrLn "Starting backend..."
  bk :: Sharded Postgres    <- start ((2,3), conninfos)
  putStrLn "Starting server..."
  serve 0xFF bk defaultSettings Nothing

conninfos = conn <$> [ "logl0", "logl1", "logl2", "logl3", "logl4",
                       "logl5", "logl6", "logl7", "logl8", "logl9" ]
 where
  conn dbname =    conninfo{ dbname=dbname, host="localhost",
                             user="logl",   password="pwned" }

