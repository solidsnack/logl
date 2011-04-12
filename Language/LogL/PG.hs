{-# LANGUAGE OverloadedStrings
           , StandaloneDeriving
           , RecordWildCards
  #-}
module Language.LogL.PG where

import Prelude hiding (unwords)
import Control.Applicative
import Data.ByteString.Char8
import Data.Monoid
import Data.Word

import Database.PQ


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

default_conninfo = Conninfo "" "" "" "" "" "" "" "" "" "" "" "" "" "" "" ""
                              "" "" "" "" "" "" "" ""

{-| Present connection info in the standard conninfo string format. 
 -}
renderconninfo              ::  Conninfo -> ByteString
renderconninfo Conninfo{..} = unwords [append k v | (k, v) <- info, v /= ""]
 where
  info = [ ("host=", host),
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

