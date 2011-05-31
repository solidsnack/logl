{-# LANGUAGE OverloadedStrings
           , RecordWildCards
           , TupleSections
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
import Network.Wai (Request(..))
import qualified Network.Wai as Web
import qualified Network.Wai.Handler.Warp as Web


wai                         ::  Web.Application
wai Request{..}              =  methodCheck
 where
  methodCheck                =  case (Web.parseMethod requestMethod) of
--  Right Web.GET           ->  if pathInfo /= [] then badPath
--                                                else get
    Right Web.POST          ->  if pathInfo /= [] then badPath
                                                  else post
    Right Web.HEAD          ->  if pathInfo /= [] then badPath
                                                  else head
    Right _                 ->  unhandledMethod
    Left _                  ->  unknownMethod


badPath                      =  http404 []
unhandledMethod              =  http405 [("Allow", "POST, GET, HEAD")]
unknownMethod                =  http400 []

get                          =  http200 [contentHTML] ""
post                         =  http200 [contentYAML] "good: POST\n"
head                         =  http200 [contentHTML] ""


http400 headers              =  response Web.status400 headers mempty
http404 headers              =  response Web.status404 headers mempty
http405 headers              =  response Web.status405 headers mempty
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

