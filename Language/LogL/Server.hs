{-# LANGUAGE OverloadedStrings
           , RecordWildCards
           , TupleSections
           , TemplateHaskell
           , TransformListComp
           , NoMonomorphismRestriction
           , StandaloneDeriving
  #-}

module Language.LogL.Server where

import Prelude hiding (head, length)
import Control.Applicative
import Control.Arrow (first)
import Control.Exception
import Control.Failure
import Control.Monad
import Control.Monad.Error
import Control.Monad.Trans
import Control.Monad.IO.Class (liftIO)
import Data.ByteString.Char8 (unpack, pack, ByteString)
import qualified Data.ByteString
import Data.Maybe
import Data.Monoid
import Data.Typeable
import Data.Word
import GHC.Exts (sortWith)
import Network.Socket
import System.IO
import System.Time
import System.Locale

import qualified Blaze.ByteString.Builder as Blaze
import qualified Control.Failure as Failure
import Data.CaseInsensitive (CI)
import qualified Data.Enumerator as Enumerator hiding (head)
import qualified Data.Enumerator.List as Enumerator
import qualified Data.Object as YAML
import qualified Data.Object.Yaml as YAML
import qualified Network.HTTP.Types as Web
import qualified Network.Wai as Web
import qualified Network.Wai.Handler.Warp as Web

import Language.LogL.Threether
import qualified Language.LogL.Macros as Macros
import Language.LogL.YAML (ToYAML(..))
import qualified Language.LogL.YAML as YAML
import qualified Language.LogL.FlatYAML as YAML
import Language.LogL.Backend
import Language.LogL.Interpret
import qualified Language.LogL.Pickle as Pickle
import qualified Language.LogL.UUID as UUID


serve :: (Backend b) -------------------------------------------------------
      => Word32 -> b -> Handle -> Web.Settings -> Maybe Socket -> IO ()
serve nBytes b logHandle settings socket = case socket of
  Nothing                   ->  Web.runSettings        settings          app
  Just socket               ->  Web.runSettingsSocket  settings  socket  app
 where
  app                        =  wai nBytes b logHandle

wai :: (Backend b) => Word32 -> b -> Handle -> Web.Application
wai nBytes b logHandle Web.Request{..} = do
  (logs, response)          <-  handled
  log (renderLogs (mappend accounting logs))
  respond response
 where
  log                        =  liftIO . Data.ByteString.hPutStrLn logHandle
  accounting                 =  case accountToken requestHeaders of
    Just token              ->  Logs [("accounting", YAML.sy token)]
    Nothing                 ->  Logs []
  handled                    =  case Web.parseMethod requestMethod of
    Right Web.GET           ->  if pathInfo /= [] then withLog badPath
                                                  else withLog hello
    Right Web.HEAD          ->  if pathInfo /= [] then withLog badPath
                                                  else withLog head
    Right Web.POST          ->  if pathInfo == ["interpret"]
                                  then  if contentYAML `elem` requestHeaders
                                          then  interpretReq b nBytes
                                          else  withLog http415
                                  else  withLog badPath
    Right _                 ->  withLog unhandledMethod
    Left _                  ->  withLog unhandledMethod
   where
    withLog response         =  return (responseLog response, response)

{-| Extracts the accounting token from headers, accepting @From@ or
    @X-Accounting@. If both are present, the latter is preferred.
 -}
accountToken                ::  [Web.Header] -> Maybe ByteString
accountToken hdrs = listToMaybe [ asciiB v | (k, v) <- hdrs,
                                             elem k accountingHeaders,
                                             then sortWith by k, then reverse ]
 where
  asciiB                    ::  Web.Ascii -> ByteString
  asciiB                     =  id -- Nonce function.

accountingHeaders           ::  [CI Web.Ascii]
accountingHeaders            =  ["From", "X-Accounting"]


interpretReq :: (MonadIO m, Backend b) => b -> Word32 ------------------------
             -> (Enumerator.Iteratee ByteString m (Logs, Response))
interpretReq backend nBytes  =  flip Enumerator.catchError err $ do
  bytes                     <-  takeOnlyNBytes nBytes
  yaml                      <-  threetherOK (excReq "Bad YAML parse.")
                                            (decode bytes)
  YAML.Request task         <-  threether (Enumerator.throwError)
                                          (excReq "Bad YAML parse.")
                                          (return)
                                          (parseRequest yaml)
  (stat, accounting)        <-  liftIO $ interpret backend task
  let log                    =  Logs [("resources", YAML.oYAML accounting)]
      response               =  sendYAML (YAML.statYAML task stat)
  return (log, response)
 where
  decode :: ByteString -> Threether YAML.ParseException YAML.YamlObject
  decode                     =  YAML.decode
  err e = case fromException e :: Maybe RequestException of
    Just (RequestException msg) -> return (errLog, http)
     where
      errLog                 =  errYAML msg
      http                   =  http400 [contentYAML] (renderLogs errLog)
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


data Response = Response Web.Status Web.ResponseHeaders Blaze.Builder

responseLog                 ::  Response -> Logs
responseLog (Response Web.Status{..} _ _) = (Logs . (:[]))
  ( if statusCode < 400 then "status" else "error",
    (YAML.sy . pack . show) statusCode              )

badPath                      =  http404 []
unhandledMethod              =  http405 [("Allow", "POST, GET, HEAD")]
internalServerError          =  http500 []

sendYAML yaml                =  http200 [contentYAML] (YAML.encode yaml)
head                         =  http200 [contentHTML] ""
hello = http200 [contentHTML] $(Macros.text "./html/hello.html")

http400 headers msg          =  Response Web.status400 headers (blaze msg)
http404 headers              =  Response Web.status404 headers mempty
http405 headers              =  Response Web.status405 headers mempty
http415                      =  Response     status415 []      mempty
http500 headers              =  Response Web.status500 headers mempty
http200 headers msg          =  Response Web.status200 headers (blaze msg)

status415 = Web.Status{statusCode=415, statusMessage="Unsupported Media Type"}


respond                     ::  (MonadIO m) => Response -> m Web.Response
respond (Response status headers builder) = do
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

data Logs                    =  Logs [(ByteString, YAML.YamlObject)]
deriving instance Eq Logs
deriving instance Show Logs
instance Monoid Logs where
  mempty                     =  Logs []
  Logs a `mappend` Logs b    =  Logs (mappend a b)

renderLogs (Logs l)          =  flatYAML (YAML.Mapping (first YAML.s <$> l))

errYAML msg                  =  Logs [("error", YAML.sy msg)]

flatYAML                     =  Blaze.toByteString . YAML.encodeFlat

