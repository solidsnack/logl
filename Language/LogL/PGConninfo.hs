{-# LANGUAGE OverloadedStrings
           , StandaloneDeriving
           , RecordWildCards
  #-}
module Language.LogL.PGConninfo where

import Prelude hiding (unwords)
import Control.Applicative
import Data.ByteString.Char8
import Data.Monoid

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
               tty          ::  ByteString,
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

empty = Conninfo "" "" "" "" "" "" "" "" "" "" "" "" "" "" "" "" "" "" "" ""
                 "" "" "" "" ""


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
           ("tty=", tty),
           ("sslmode=", sslmode),
           ("sslcert=", sslcert),
           ("sslkey=", sslkey),
           ("sslrootcert=", sslrootcert),
           ("sslcrl=", sslcrl),
           ("sslrequirepeer=", sslrequirepeer),
           ("krbsrvname=", krbsrvname),
           ("gsslib=", gsslib),
           ("service=", service) ]

