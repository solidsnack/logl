
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
    CREATE TABLE   "Log/L,v2011-03-25".log
      ( uuid        uuid PRIMARY KEY,
        timestamp   timestamp with time zone NOT NULL,
        tag         bytea CHECK (length(tag) <= 128) NOT NULL,
        tombstone   timestamp with time zone DEFAULT NULL      );
    CREATE INDEX   "log/timestamp" ON "Log/L,v2011-03-25".log (timestamp);
    CREATE INDEX   "log/tag" ON "Log/L,v2011-03-25".log (tag);
    CREATE INDEX   "log/tombstone" ON "Log/L,v2011-03-25".log (tombstone);
    RETURN NEXT    'Log/L,v2011-03-25.log';
  EXCEPTION WHEN duplicate_table THEN END;
  BEGIN
    CREATE TABLE   "Log/L,v2011-03-25".entry
      ( uuid        uuid PRIMARY KEY,
        log         uuid NOT NULL,
        timestamp   timestamp with time zone NOT NULL,
        client_time timestamp with time zone NOT NULL,
        tag         bytea CHECK (length(tag) <= 128) NOT NULL,
        bytes       bytea NOT NULL                             );
    CREATE INDEX   "entry/timestamp" ON "Log/L,v2011-03-25".entry (timestamp);
    CREATE INDEX   "entry/log,client_time"
              ON   "Log/L,v2011-03-25".entry (log,client_time);
    CREATE INDEX   "entry/log" ON "Log/L,v2011-03-25".entry (log);
    RETURN NEXT    'Log/L,v2011-03-25.entry';
  EXCEPTION WHEN duplicate_table THEN END;
  BEGIN
    CREATE VIEW    "Log/L,v2011-03-25".entry_with_tombstone AS
      SELECT entry.*, log.tombstone
        FROM "Log/L,v2011-03-25".entry entry, "Log/L,v2011-03-25".log log
       WHERE entry.log = log.uuid;
    RETURN NEXT    'Log/L,v2011-03-25.entry_with_tombstone';
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

