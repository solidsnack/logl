
--  Idempotent setup of schema, tables and type shim tables.
DROP FUNCTION IF EXISTS "Log/L,v2011-03-25#setup"();
CREATE OR REPLACE FUNCTION "Log/L,v2011-03-25#setup"()
  RETURNS SETOF text AS $$
BEGIN
  BEGIN
    CREATE SCHEMA  "Log/L,v2011-03-25";
    RETURN NEXT    'Log/L,v2011-03-25';
  EXCEPTION WHEN duplicate_schema THEN END;
  BEGIN
    CREATE TABLE   "Log/L,v2011-03-25".logs
      ( uuid        uuid PRIMARY KEY
      , time        timestamp with time zone NOT NULL
      , destroy     boolean NOT NULL
      , tag         bytea CHECK (length(tag) <= 128) NOT NULL ); 
    CREATE INDEX   "logs/tag" ON "Log/L,v2011-03-25".logs (tag);
    CREATE INDEX   "logs/time" ON "Log/L,v2011-03-25".logs (time);
    RETURN NEXT    'Log/L,v2011-03-25.logs';
  EXCEPTION WHEN duplicate_table THEN END;
  BEGIN
    CREATE TABLE   "Log/L,v2011-03-25".entries
      ( uuid        uuid NOT NULL
      , log         uuid NOT NULL
      , user_time   timestamp with time zone NOT NULL
      , time        timestamp with time zone NOT NULL
      , data        bytea NOT NULL
      ,             PRIMARY KEY (uuid, log)           ); 
    CREATE INDEX   "entries/log,user_time"
              ON   "Log/L,v2011-03-25".entries (log, user_time);
    CREATE INDEX   "entries/log" ON "Log/L,v2011-03-25".entries (log);
    CREATE INDEX   "entries/time" ON "Log/L,v2011-03-25".entries (time);
    RETURN NEXT    'Log/L,v2011-03-25.entries';
  EXCEPTION WHEN duplicate_table THEN END;
  BEGIN
    CREATE TABLE   "Log/L,v2011-03-25".entries_with_bool
      ( okay        boolean NOT NULL )
    INHERITS ("Log/L,v2011-03-25".entries);
    RETURN NEXT    'Log/L,v2011-03-25.entries_with_bool';
  EXCEPTION WHEN duplicate_table THEN END;
END;
$$ LANGUAGE plpgsql;
SELECT "Log/L,v2011-03-25#setup"();

--  Return a single entry.
DROP FUNCTION IF EXISTS "Log/L,v2011-03-25".get_entry(uuid, uuid);
CREATE OR REPLACE FUNCTION "Log/L,v2011-03-25".get_entry(uuid, uuid)
  RETURNS "Log/L,v2011-03-25".entries_with_bool AS $$
SELECT "Log/L,v2011-03-25".entries.*, NOT("Log/L,v2011-03-25".logs.destroy)
     FROM "Log/L,v2011-03-25".entries, "Log/L,v2011-03-25".logs
    WHERE "Log/L,v2011-03-25".entries.uuid = $2
      AND "Log/L,v2011-03-25".entries.log = $1
      AND "Log/L,v2011-03-25".logs.uuid = $1;
$$ LANGUAGE sql STRICT;

--  Returns entries following a particular entry, in the given time range.
--  The UUID of the last returned row forms a "natural cursor" that allows
--  the end user to restart their query.
DROP FUNCTION IF EXISTS
 "Log/L,v2011-03-25".search_entries( uuid, uuid, timestamp with time zone
                                               , timestamp with time zone );
CREATE OR REPLACE FUNCTION
 "Log/L,v2011-03-25".search_entries( uuid, uuid, timestamp with time zone
                                               , timestamp with time zone )
  RETURNS SETOF "Log/L,v2011-03-25".entries_with_bool AS $$
SELECT "Log/L,v2011-03-25".entries.*, NOT("Log/L,v2011-03-25".logs.destroy)
     FROM "Log/L,v2011-03-25".entries, "Log/L,v2011-03-25".logs
    WHERE "Log/L,v2011-03-25".entries.log = $1
      AND "Log/L,v2011-03-25".entries.user_time >= $3
      AND "Log/L,v2011-03-25".entries.user_time <= $4
      AND "Log/L,v2011-03-25".entries.uuid > $2
      AND "Log/L,v2011-03-25".logs.uuid = $1
 ORDER BY ( "Log/L,v2011-03-25".entries.user_time
          , "Log/L,v2011-03-25".entries.uuid      ) ASC
    LIMIT 256;
$$ LANGUAGE sql STRICT ROWS 256;

