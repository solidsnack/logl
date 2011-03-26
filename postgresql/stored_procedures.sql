
--  Idempotent setup of schema, tables and types.
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
      , tombstone   timestamp with time zone DEFAULT NULL
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
    CREATE TYPE    "Log/L,v2011-03-25".entries_with_bool AS
      ( uuid        uuid
      , log         uuid
      , user_time   timestamp with time zone
      , time        timestamp with time zone
      , data        bytea
      , tombstone   timestamp with time zone );
    RETURN NEXT    'Log/L,v2011-03-25.entries_with_bool';
  EXCEPTION WHEN duplicate_table THEN END;
END;
$$ LANGUAGE plpgsql;
SELECT "Log/L,v2011-03-25#setup"();

--  Returns a single row that is either the entry requested or "tombstone"
--  indicating the log was destroyed. Empty output indicates this server has
--  no knowledge of the log entry or the log.
DROP FUNCTION IF EXISTS "Log/L,v2011-03-25".get_entry(uuid, uuid);
CREATE OR REPLACE FUNCTION "Log/L,v2011-03-25".get_entry(uuid, uuid)
  RETURNS SETOF "Log/L,v2011-03-25".entries_with_bool AS $$
DECLARE
  result "Log/L,v2011-03-25".entries_with_bool;
BEGIN
  SELECT NULL, NULL, NULL, NULL, NULL, "Log/L,v2011-03-25".logs.tombstone
    INTO result
    FROM "Log/L,v2011-03-25".logs
   WHERE uuid = $1 AND tombstone IS NOT NULL;
  IF FOUND THEN
    RETURN NEXT result;
  ELSE
    RETURN QUERY SELECT "Log/L,v2011-03-25".entries.*,
                        NULL :: timestamp with time zone
                   FROM "Log/L,v2011-03-25".entries
                  WHERE "Log/L,v2011-03-25".entries.uuid = $2
                    AND "Log/L,v2011-03-25".entries.log = $1;
  END IF;
END;
$$ LANGUAGE plpgsql;

--  Returns entries following a particular entry, in the given time range.
--  The UUID of the last returned row forms a "natural cursor" that allows
--  the end user to restart their query. If this function returns rows, it
--  returns either log entries or a "tombstone" indicating that the log was
--  destroyed.
DROP FUNCTION IF EXISTS
 "Log/L,v2011-03-25".search_entries( uuid, uuid, timestamp with time zone
                                               , timestamp with time zone );
CREATE OR REPLACE FUNCTION
 "Log/L,v2011-03-25".search_entries( uuid, uuid, timestamp with time zone
                                               , timestamp with time zone )
  RETURNS SETOF "Log/L,v2011-03-25".entries_with_bool AS $$
DECLARE
  result "Log/L,v2011-03-25".entries_with_bool;
BEGIN
  SELECT NULL, NULL, NULL, NULL, NULL, "Log/L,v2011-03-25".logs.tombstone
    INTO result
    FROM "Log/L,v2011-03-25".logs
   WHERE uuid = $1 AND tombstone IS NOT NULL;
  IF FOUND THEN
    RETURN NEXT result;
  ELSE
    RETURN QUERY SELECT "Log/L,v2011-03-25".entries.*,
                        NULL :: timestamp with time zone
                   FROM "Log/L,v2011-03-25".entries
                  WHERE log = $1 AND user_time >= $3 AND user_time <= $4
                    AND uuid > $2
               ORDER BY (user_time, uuid) ASC
                  LIMIT 256;
  END IF;
END;
$$ LANGUAGE plpgsql ROWS 256;

