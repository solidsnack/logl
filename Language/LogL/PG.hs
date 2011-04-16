{-# LANGUAGE OverloadedStrings
           , StandaloneDeriving
           , RecordWildCards
           , ParallelListComp
  #-}
module Language.LogL.PG where

import Prelude hiding (unwords, length, take)
import Control.Applicative
import Control.Exception
import Control.Monad.Error
import Data.ByteString.Char8
import Data.Either
import Data.Monoid
import Data.Word

import Database.PQ

import Language.LogL.Syntax
import qualified Language.LogL.Pickle as Pickle


--  Example of using libpq:
--  conn <- connectdb ""
--  trace conn System.IO.stderr
--  Just res <- exec conn "SELECT * FROM logl.write_log('00000000-0000-0000-0000-000000000000','2011-04-07 03:05:49.105519 UTC','2011-04-07 03:05:49.105519 UTC','');"
--  Just res <- execParams conn "SELECT * FROM logl.log WHERE uuid = $1;" [Just (0, "00000000-0000-0000-0000-000000000000", Text)] Text
--  resultStatus res
--  getvalue res (toRow 0) (toColumn 0)

guard :: ByteString -------------------------------------------------
      -> Maybe Result -> IO (Either ByteString Result)
guard rem Nothing = return . Left $ mappend "No response from server. " rem
guard rem (Just result)      =  do
  stat                      <-  resultStatus result
  msg                       <-  maybe "" id <$> resultErrorMessage result
  let errmsg                 =  mconcat ["Query failed. ", rem, " ", msg]
  return $ case stat of
    BadResponse             ->  Left errmsg
    FatalError              ->  Left errmsg
    _                       ->  Right result


call                        ::  ByteString -> Word -> ByteString
call name arity              =  mconcat
  [ "SELECT * FROM ", name, "(",
    intercalate ", " ((cons '$' . pack . show) <$> [1..arity]),
    ");"                                                       ]


{-| PG Connection info string as a record. 
 -}
data Conninfo
  = Conninfo { host         ::  ByteString,
               hostaddr     ::  ByteString,
               port         ::  ByteString,
               dbname       ::  ByteString,
               user         ::  ByteString,
               password     ::  ByteString,
               connection_timeout :: ByteString,
               client_encoding :: ByteString,
               options      ::  ByteString,
               application_name :: ByteString,
               fallback_application_name :: ByteString,
               keepalives   ::  ByteString,
               keepalives_idle :: ByteString,
               keepalives_interval :: ByteString,
               keepalives_count :: ByteString,
               sslmode      ::  ByteString,
               sslcert      ::  ByteString,
               sslkey       ::  ByteString,
               sslrootcert  ::  ByteString,
               sslcrl       ::  ByteString,
               sslrequirepeer :: ByteString,
               krbsrvname   ::  ByteString,
               gsslib       ::  ByteString,
               service      ::  ByteString                }
deriving instance Eq Conninfo
deriving instance Show Conninfo

{-| A connnection info object with no parameters set.
 -}
default_conninfo            ::  Conninfo
default_conninfo = Conninfo "" "" "" "" "" "" "" "" "" "" "" "" "" "" "" ""
                            "" "" "" "" "" "" "" ""

{-| Present connection info in the standard conninfo string format. 
 -}
renderconninfo              ::  Conninfo -> ByteString
renderconninfo Conninfo{..}  =  unwords [append k v | (k, v) <- info, v /= ""]
 where
  info = [ ("host=", host), -------------------------------------------------
           ("hostaddr=", hostaddr),
           ("port=", port),
           ("dbname=", dbname),
           ("user=", user),
           ("password=", password),
           ("connection_timeout=", connection_timeout),
           ("client_encoding=", client_encoding),
           ("options=", options),
           ("application_name=", application_name),
           ("fallback_application_name=", fallback_application_name),
           ("keepalives=", keepalives),
           ("keepalives_idle=", keepalives_idle),
           ("keepalives_interval=", keepalives_interval),
           ("keepalives_count=", keepalives_count),
           ("sslmode=", sslmode),
           ("sslcert=", sslcert),
           ("sslkey=", sslkey),
           ("sslrootcert=", sslrootcert),
           ("sslcrl=", sslcrl),
           ("sslrequirepeer=", sslrequirepeer),
           ("krbsrvname=", krbsrvname),
           ("gsslib=", gsslib),
           ("service=", service) ]


class PGPickle t where
  pqARGV                    ::  t -> [Maybe (Oid, ByteString, Format)]
  fromResult                ::  Result -> IO [Either ByteString t]

instance PGPickle Log where
  pqARGV (Log i t ct tag)    =  [ Just (0, Pickle.o i,   Text),
                                  Just (0, Pickle.o t,   Text),
                                  Just (0, Pickle.o ct,  Text),
                                  Just (0, Pickle.o tag, Binary) ]
  fromResult result          =  mapM fromTuple =<< unpackResult result
   where
    fromTuple tuple          =  case tuple of
      [Just i, Just t, Just ct, Just tag] -> do
        tag'                <-  unescapeBytea tag
        maybe err ioRight $ do uuid'         <-  Pickle.i i
                               timestamp'    <-  Pickle.i t
                               client_time'  <-  Pickle.i ct
                               tag''         <-  Pickle.i =<< tag'
                               Just (Log uuid' timestamp' client_time' tag'')
      _                     ->  err
     where
      err = ioLeft ("Bad SQL tuple for Log: " `mappend` showTuple tuple)


instance PGPickle Entry where
  pqARGV Entry{..}           =  [ Just (0, Pickle.o uuid,        Text),
                                  Just (0, Pickle.o log,         Text),
                                  Just (0, Pickle.o parent,      Text),
                                  Just (0, Pickle.o timestamp,   Text),
                                  Just (0, Pickle.o client_time, Text),
                                  Just (0, Pickle.o tag,         Binary),
                                  Just (0, bytes,                Binary) ]
  fromResult result          =  mapM fromTuple =<< unpackResult result
   where
    fromTuple tuple          =  case tuple of
      [ Just uuid, Just log, Just parent, Just timestamp,
        Just client_time, Just tag, Just bytes           ] -> do
        tag'                <-  unescapeBytea tag
        bytes'              <-  unescapeBytea bytes
        maybe err ioRight $ do uuid'         <-  Pickle.i uuid
                               log'          <-  Pickle.i log
                               parent'       <-  Pickle.i parent
                               timestamp'    <-  Pickle.i timestamp
                               client_time'  <-  Pickle.i client_time
                               tag''         <-  Pickle.i =<< tag'
                               bytes''       <-  Pickle.i =<< bytes'
                               Just ( Entry uuid' log' parent' timestamp'
                                            client_time' tag'' bytes''    )
      _                     ->  err
     where
      err = ioLeft ("Bad SQL tuple for Entry: " `mappend` showTuple tuple)


unpackResult                ::  Result -> IO [[Maybe ByteString]]
unpackResult result          =  do
  rows                      <-  ntuples result
  columns                   <-  nfields result
  (sequence . (sequence <$>))
    [ [ getvalue' result r c | c <- toColumn <$> [0..(columns - 1)] ]
                             | r <- toRow <$> [0..(rows - 1)]       ]


showTuple                   ::  [Maybe ByteString] -> ByteString
showTuple tuple | tooBig     =  take 1021 text `mappend` "..."
                | otherwise  =  text
 where
  tooBig                     =  length text > 1024
  text                       =  unwords (render <$> tuple)
  render (Just bytes)        =  (pack . show) bytes
  render Nothing             =  "!"


ioLeft                       =  return . Left

ioRight                      =  return . Right


deriving instance Eq ConnStatus

