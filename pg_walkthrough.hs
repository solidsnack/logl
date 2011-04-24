{-# LANGUAGE OverloadedStrings
  #-}
{- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

  Walkthrough of PG backend.

 .  Load file in GHCi:

    > \i ./pg_walkthrough.hs

 .  Connect to Postgres using the default ident auth or passing conninfo
    params as needed:

    > postgres <- local_pg
    > postgres <- start PG.Conninfo{user="...", password="...", dbname="..."}

 .  Check your connection:

    > PG.connectPoll (conn postgres)
    PollingOk
    it :: PG.PollingStatus

 .  Insert the sample data:

    > insert_samples postgres

 .  Connect to your server with psql and examine the tables:

    postgres=> \x
    postgres=> SELECT * FROM logl.entry_with_pointers;
    -[ RECORD 1 ]-------------------------------------
    log         | aaaaaaa0-bbbb-cccc-dddd-eeeeeeeeeeee
    parent      | aaaaaaa0-bbbb-cccc-dddd-eeeeeeeeeeee
    uuid        | aaaaaaaa-bbb0-cccc-dddd-eeeeeeeeeeee
    timestamp   | 2011-04-07 03:05:49.112+00
    client_time | 2011-04-07 03:05:49+00
    tag         | entry0
    bytes       | data0
    -[ RECORD 2 ]-------------------------------------
    log         | aaaaaaa0-bbbb-cccc-dddd-eeeeeeeeeeee
    parent      | aaaaaaaa-bbb0-cccc-dddd-eeeeeeeeeeee
    uuid        | aaaaaaaa-bbb1-cccc-dddd-eeeeeeeeeeee
    timestamp   | 2011-04-07 03:05:49.54772+00
    client_time | 2011-04-07 03:05:49.491091+00
    tag         | entry1
    bytes       | data1

 .  Clean up everything.

    postgres=> DROP SCHEMA logl CASCADE;

 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -}


import qualified Database.PQ as PG

import Language.LogL.Backend
import qualified Language.LogL.PG as PG
import Language.LogL.Syntax


local_pg                    ::  IO Postgres
local_pg                     =  start PG.conninfo


insert_samples postgres      =  do
  run postgres (WriteLog log0)
  run postgres (WriteEntry entry0)
  run postgres (WriteEntry entry1)


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

