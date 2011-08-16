{-# LANGUAGE OverloadedStrings
           , RecordWildCards
           , TupleSections
           , TemplateHaskell
           , NoMonomorphismRestriction
           , StandaloneDeriving
  #-}

module Language.LogL.Server where

import Prelude hiding (head, length)
import Control.Applicative
import Control.Exception
import Control.Monad
import Control.Monad.Error
import Control.Monad.Trans
import Control.Monad.IO.Class (liftIO)
import Data.ByteString.Char8 (unpack, pack, ByteString)
import qualified Data.ByteString
import Data.Monoid
import Data.Typeable
import Data.Word
import Network.Socket
import System.Time
import System.Locale

import qualified Blaze.ByteString.Builder as Blaze
import qualified Control.Failure as Failure
import qualified Data.Enumerator as Enumerator hiding (head)
import qualified Data.Enumerator.List as Enumerator
import qualified Data.Object as YAML
import qualified Data.Object.Yaml as YAML
import qualified Network.HTTP.Types as Web
import qualified Network.Wai as Web
import qualified Network.Wai.Handler.Warp as Web

import Language.LogL.Threether
import qualified Language.LogL.Macros as Macros
import qualified Language.LogL.YAML as YAML
import Language.LogL.Backend
import Language.LogL.Interpret


serve :: (Backend b) => Word32 -> b -> Web.Settings -> Maybe Socket -> IO ()
serve nBytes b settings socket = case socket of
  Nothing                   ->  Web.runSettings        settings          app
  Just socket               ->  Web.runSettingsSocket  settings  socket  app
 where
  app                        =  wai nBytes b

wai                         ::  (Backend b) => Word32 -> b -> Web.Application
wai nBytes b Web.Request{..} =  methodCheck
 where
  methodCheck                =  case Web.parseMethod requestMethod of
    Right Web.GET           ->  if pathInfo /= [] then badPath else hello
    Right Web.HEAD          ->  if pathInfo /= [] then badPath else head
    Right Web.POST          ->  if pathInfo == ["interpret"]
                                  then  if contentYAML `elem` requestHeaders
                                          then  interpretReq b nBytes
                                          else  http415
                                  else  badPath
    Right _                 ->  unhandledMethod
    Left _                  ->  unhandledMethod

interpretReq backend nBytes  =  flip Enumerator.catchError send400 $ do
  bytes                     <-  takeOnlyNBytes nBytes
  yaml                      <-  threetherOK (excReq "Bad YAML parse.")
                                            (decode bytes)
  YAML.Request task         <-  threether (Enumerator.throwError)
                                          (excReq "Bad YAML parse.")
                                          (return)
                                          (parseRequest yaml)
  stat                      <-  liftIO $ interpret backend task
  sendYAML (YAML.statYAML task stat)
 where
  decode :: ByteString -> Threether YAML.ParseException YAML.YamlObject
  decode                     =  YAML.decode
  send400 e = case fromException e :: Maybe RequestException of
    Just (RequestException msg) -> http400 [contentYAML]
                                           (YAML.renderKV [("error", msg)])
    Nothing                 ->  Enumerator.throwError e

parseRequest :: YAML.YamlObject -> Threether RequestException YAML.Request
parseRequest yaml            =  do
  mapping                   <-  translateYAMLError $ YAML.fromMapping yaml
  extracted                 <-  translateYAMLError $ YAML.request mapping
  case extracted of
    Just r                  ->  return r
    Nothing                 ->  Failure.failure exc
   where
    exc                      =  RequestException "Bad data in YAML."

translateYAMLError          ::  Threether YAML.ObjectExtractError t
                            ->  Threether RequestException t
translateYAMLError           =  threetherOK (Failure.failure exc)
 where
  exc                        =  RequestException "YAML not as expected."

newtype RequestException     =  RequestException ByteString
instance Show RequestException where
  show (RequestException m)  =  "RequestException: " ++ unpack m
deriving instance Typeable RequestException
instance Exception RequestException

excReq :: (Monad m) => ByteString -> Enumerator.Iteratee a m b
excReq                       =  Enumerator.throwError . RequestException


badPath                      =  http404 []
unhandledMethod              =  http405 [("Allow", "POST, GET, HEAD")]
internalServerError          =  http500 []

sendYAML yaml                =  http200 [contentYAML] (YAML.encode yaml)
head                         =  http200 [contentHTML] ""
hello = http200 [contentHTML] $(Macros.text "./html/hello.html")

http400 headers msg          =  response Web.status400 headers (blaze msg)
http404 headers              =  response Web.status404 headers mempty
http405 headers              =  response Web.status405 headers mempty
http415                      =  response status415 [] mempty
http500 headers              =  response Web.status500 headers mempty
http200 headers msg          =  response Web.status200 headers (blaze msg)

status415 = Web.Status{statusCode=415, statusMessage="Unsupported Media Type"}


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

{-| Throw out input if it's greater than a certain size.
 -}
takeOnlyNBytes :: (Monad m) ---------------------------------------------
               => Word32 -> Enumerator.Iteratee ByteString m ByteString
takeOnlyNBytes n             =  worker (fromIntegral n) ""
 where
  worker n b                 =  do
    mb'                     <-  Enumerator.head
    case mb' of
      Just b' | len <= n    ->  worker (n-len) (mappend b b')
              | otherwise   ->  excReq "Body too large."
       where
        len                  =  Data.ByteString.length b'
      Nothing               ->  return b

