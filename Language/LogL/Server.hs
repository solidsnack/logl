{-# LANGUAGE OverloadedStrings
           , RecordWildCards
           , TupleSections
           , TemplateHaskell
           , NoMonomorphismRestriction
  #-}

module Language.LogL.Server where

import Prelude hiding (head)
import Control.Applicative
import Control.Monad
import Control.Monad.Trans
import Data.ByteString.Char8 (pack, ByteString)
import Data.Monoid
import System.Time
import System.Locale

import qualified Blaze.ByteString.Builder as Blaze
import Data.Enumerator hiding (head)
import qualified Data.Object.Yaml as YAML
import qualified Network.HTTP.Types as Web
import qualified Network.Wai as Web
import qualified Network.Wai.Handler.Warp as Web

import qualified Language.LogL.Macros as Macros
import qualified Language.LogL.YAML as YAML


wai                         ::  Web.Application
wai Web.Request{..}          =  methodCheck
--wai Web.Request{..} = catchError methodCheck (const internalServerError)
 where
  methodCheck               ::  (MonadIO m) => m Web.Response
  methodCheck                =  case (Web.parseMethod requestMethod) of
    Right Web.GET           ->  if pathInfo /= [] then badPath
                                                  else hello
    Right Web.POST          ->  case pathInfo of
      ["interpret"]         ->  post
      _                     ->  badPath
    Right Web.HEAD          ->  if pathInfo /= [] then badPath
                                                  else head
    Right _                 ->  unhandledMethod
    Left _                  ->  unknownMethod


badPath                      =  http404 []
unhandledMethod              =  http405 [("Allow", "POST, GET, HEAD")]
unknownMethod                =  http400 []
internalServerError          =  http500 []

post                         =  http200 [contentYAML] "good: POST\n"
head                         =  http200 [contentHTML] ""
hello = http200 [contentHTML] $(Macros.text "./html/hello.html")

http400 headers              =  response Web.status400 headers mempty
http404 headers              =  response Web.status404 headers mempty
http405 headers              =  response Web.status405 headers mempty
http500 headers              =  response Web.status500 headers mempty
http200 headers msg          =  response Web.status200 headers (blaze msg)


response :: (MonadIO m) ----------------------------------------------------
         => Web.Status -> [Web.Header] -> Blaze.Builder -> m Web.Response
response status headers builder = do
  date                      <-  liftIO date
  return $ Web.ResponseBuilder status (date:headers) builder


blaze                        =  Blaze.fromByteString


contentHTML                  =  ("Content-Type", "text/html;charset=UTF-8")
contentYAML                  =  ("Content-Type", "application/yaml")

date                         =  ("Date",) . pack <$> httpCurrentDate


{-| This code borrowed directly from:

      <http://hackage.haskell.org/package/hS3-0.5.1>

 -}
httpCurrentDate             ::  IO String
httpCurrentDate              =  do
  c                         <-  getClockTime
  let utc_time               =  (toUTCTime c) {ctTZName = "GMT"}
  return $ formatCalendarTime defaultTimeLocale rfc822DateFormat utc_time

