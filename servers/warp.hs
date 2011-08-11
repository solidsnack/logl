#!/usr/bin/env runhaskell
{-# LANGUAGE ScopedTypeVariables
  #-}

import Network.Wai.Handler.Warp

import Language.LogL.Backend
import Language.LogL.Server


main                         =  do
  putStrLn "Starting backend..."
  stdout :: STDOUT          <-  start ()
  putStrLn "Starting server..."
  serve 0xFF stdout defaultSettings Nothing


