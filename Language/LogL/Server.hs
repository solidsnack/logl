{-# LANGUAGE OverloadedStrings
           , RecordWildCards
  #-}

module Language.LogL.Server where

import Control.Monad
import Data.ByteString.Char8()

import qualified Network.HTTP.Types as Web
import qualified Network.Wai as Web
import qualified Network.Wai.Handler.Warp as Web
import qualified Blaze.ByteString.Builder as Blaze


wai                         ::  Web.Application
wai Web.Request{..}          =  methodCheck
 where
  methodCheck                =  case (Web.parseMethod requestMethod) of
    Right Web.GET           ->  if pathInfo /= [] then return badPath
                                                  else return get
    Right Web.POST          ->  if pathInfo /= [] then return badPath
                                                  else return post
    Right _                 ->  return unhandledMethod
    Left _                  ->  return unknownMethod


badPath                      =  http400 [] "error: bad path\n"
unhandledMethod              =  http400 [] "error: unhandled method\n"
unknownMethod                =  http400 [] "error: unknown method\n"

get                          =  http200 [] "good: GET\n"
post                         =  http200 [] "good: POST\n"


http400 headers msg = Web.ResponseBuilder Web.status400 headers (blaze msg)

http200 headers msg = Web.ResponseBuilder Web.status200 headers (blaze msg)


blaze                        =  Blaze.fromByteString

