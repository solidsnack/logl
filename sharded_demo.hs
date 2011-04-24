
--  Database setup for Postgres:
--- CREATE ROLE logl WITH LOGIN UNENCRYPTED PASSWORD 'pwned';
--- CREATE DATABASE logl0 WITH OWNER logl;
--- CREATE DATABASE logl1 WITH OWNER logl;
--- CREATE DATABASE logl2 WITH OWNER logl;
--- CREATE DATABASE logl3 WITH OWNER logl;
--- CREATE DATABASE logl4 WITH OWNER logl;
--- CREATE DATABASE logl5 WITH OWNER logl;
--- CREATE DATABASE logl6 WITH OWNER logl;
--- CREATE DATABASE logl7 WITH OWNER logl;
--- CREATE DATABASE logl8 WITH OWNER logl;
--- CREATE DATABASE logl9 WITH OWNER logl;

import Control.Applicative

import Language.LogL.Backend
import Language.LogL.PG
import Language.LogL.Syntax


conninfos = conn <$> [ "logl0", "logl1", "logl2", "logl3", "logl4",
                       "logl5", "logl6", "logl7", "logl8", "logl9" ]
 where
  conn dbname = default_conninfo{ dbname=dbname, host="localhost",
                                  user="logl", password="pwned"    }

default_sharded             ::  IO (Sharded Postgres)
default_sharded              =  start ((2,3), conninfos)


insert_samples backend       =  do
  run backend (WriteLog log0)
  run backend (WriteEntry entry0)
  run backend (WriteEntry entry1)


log0                         =  Log "aaaaaaa0-bbbb-cccc-dddd-eeeeeeeeeeee"
                                    "2011-04-07 03:05:49.11209 UTC"
                                    "2011-04-07 03:05:49.0037 UTC"
                                    "log0"

entry0                       =  Entry "aaaaaaaa-bbb0-cccc-dddd-eeeeeeeeeeee"
                                      "aaaaaaa0-bbbb-cccc-dddd-eeeeeeeeeeee"
                                      "aaaaaaa0-bbbb-cccc-dddd-eeeeeeeeeeee"
                                      "2011-04-07 03:05:49.112 UTC"
                                      "2011-04-07 03:05:49.000 UTC"
                                      "entry0"
                                      "data0"
entry1                       =  Entry "aaaaaaaa-bbb1-cccc-dddd-eeeeeeeeeeee"
                                      "aaaaaaa0-bbbb-cccc-dddd-eeeeeeeeeeee"
                                      "aaaaaaaa-bbb0-cccc-dddd-eeeeeeeeeeee"
                                      "2011-04-07 03:05:49.54772 UTC"
                                      "2011-04-07 03:05:49.491091 UTC"
                                      "entry1"
                                      "data1"

