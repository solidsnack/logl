
--  Idempotent setup of schema, tables and types.
DROP FUNCTION IF EXISTS "logl#setup"();
CREATE OR REPLACE FUNCTION "logl#setup"()
  RETURNS SETOF text AS $$
BEGIN
  BEGIN
    CREATE SCHEMA   logl;
    RETURN NEXT    'logl';
  EXCEPTION WHEN duplicate_schema THEN END;
  BEGIN
    CREATE TABLE    logl.tombstone
      ( log         uuid PRIMARY KEY,
        timestamp   timestamp with time zone NOT NULL );
    CREATE INDEX   "tombstone/timestamp"
              ON    logl.tombstone (timestamp);
    RETURN NEXT    'logl.tombstone';
  EXCEPTION WHEN duplicate_table THEN END;
  BEGIN
    CREATE TABLE    logl.log
      ( uuid        uuid PRIMARY KEY,
        timestamp   timestamp with time zone NOT NULL,
        tag         bytea CHECK (length(tag) <= 128) NOT NULL );
    CREATE INDEX   "log/timestamp" ON  logl.log (timestamp);
    CREATE INDEX   "log/tag" ON  logl.log (tag);
    RETURN NEXT    'logl.log';
  EXCEPTION WHEN duplicate_table THEN END;
  BEGIN
    CREATE VIEW     logl.log_with_tombstone AS
      SELECT log.*, tombstone.timestamp AS tombstone
        FROM logl.log AS log LEFT OUTER JOIN
             logl.tombstone AS tombstone
          ON (log.uuid = tombstone.log);
    RETURN NEXT    'logl.log_with_tombstone';
  EXCEPTION WHEN duplicate_table THEN END;
  BEGIN
    CREATE TABLE    logl.entry
      ( uuid        uuid PRIMARY KEY,
        log         uuid NOT NULL,
        timestamp   timestamp with time zone NOT NULL,
        client_time timestamp with time zone NOT NULL,
        tag         bytea CHECK (length(tag) <= 128) NOT NULL,
        bytes       bytea NOT NULL                             );
    CREATE INDEX   "entry/timestamp" ON  logl.entry (timestamp);
    CREATE INDEX   "entry/log,client_time"
              ON    logl.entry (log,client_time);
    CREATE INDEX   "entry/log" ON  logl.entry (log);
    RETURN NEXT    'logl.entry';
  EXCEPTION WHEN duplicate_table THEN END;
  BEGIN
    CREATE VIEW     logl.entry_with_tombstone AS
      SELECT entry.*, tombstone.timestamp AS tombstone
        FROM logl.entry AS entry LEFT OUTER JOIN
             logl.tombstone AS tombstone
          ON (entry.log = tombstone.log);
    RETURN NEXT    'logl.entry_with_tombstone';
  EXCEPTION WHEN duplicate_table THEN END;
END;
$$ LANGUAGE plpgsql;
SELECT "logl#setup"();

--  WriteLog                  ::  Log -> Task ()
CREATE OR REPLACE FUNCTION logl.write_log( uuid, timestamp with time zone,
                                           bytea )
RETURNS VOID AS $$
  INSERT INTO logl.log VALUES ($1, $2, $3);
$$ LANGUAGE sql;

--  WriteEntry                ::  Entry -> Task ()
CREATE OR REPLACE FUNCTION logl.write_entry( uuid, uuid,
                                             timestamp with time zone,
                                             timestamp with time zone,
                                             bytea, bytea              )
RETURNS VOID AS $$
  INSERT INTO logl.entry VALUES ($1, $2, $3, $4, $5, $6);
$$ LANGUAGE sql;

--  WriteTombstone            ::  ID Log -> Task ()
CREATE OR REPLACE FUNCTION logl.write_tombstone(uuid, timestamp with time zone)
RETURNS VOID AS $$
  INSERT INTO logl.tombstone VALUES ($1, $2);
$$ LANGUAGE sql;

--  LookupLog                 ::  ID Log -> Task Log
CREATE OR REPLACE FUNCTION logl.lookup_log(uuid)
  RETURNS SETOF logl.log_with_tombstone AS $$
DECLARE
  result logl.log_with_tombstone;
BEGIN
  SELECT * INTO result
    FROM logl.log_with_tombstone
   WHERE uuid = $1;
  IF FOUND THEN
    IF result.tombstone IS NOT NULL THEN
      SELECT result.uuid, NULL, NULL, result.tombstone INTO result;
    END IF;
    RETURN NEXT result;
  END IF;
END;
$$ LANGUAGE plpgsql;

--  LookupEntry               ::  ID Entry -> Task Entry
CREATE OR REPLACE FUNCTION logl.lookup_entry(uuid)
  RETURNS SETOF logl.entry_with_tombstone AS $$
DECLARE
  result logl.entry_with_tombstone;
BEGIN
  SELECT * INTO result
    FROM logl.entry_with_tombstone
   WHERE uuid = $1;
  IF FOUND THEN
    IF result.tombstone IS NOT NULL THEN
      SELECT NULL, result.log, NULL, NULL, NULL, NULL, result.tombstone
        INTO result;
    END IF;
    RETURN NEXT result;
  END IF;
END;
$$ LANGUAGE plpgsql;

--  Returns entries following a particular entry, in the given time range.
--  The UUID of the last returned row forms a "natural cursor" that allows
--  the end user to restart their query. If this function returns rows, it
--  returns either log entries or a "tombstone" indicating that the log was
--  destroyed.
--CREATE OR REPLACE FUNCTION
--  logl.search_entries( uuid, uuid, timestamp with time zone
--                                               , timestamp with time zone )
--  RETURNS SETOF  logl.entries_with_bool AS $$
--DECLARE
--  result  logl.entries_with_bool;
--BEGIN
--  SELECT NULL, NULL, NULL, NULL, NULL,  logl.logs.tombstone
--    INTO result
--    FROM  logl.logs
--   WHERE uuid = $1 AND tombstone IS NOT NULL;
--  IF FOUND THEN
--    RETURN NEXT result;
--  ELSE
--    RETURN QUERY SELECT  logl.entries.*,
--                        NULL :: timestamp with time zone
--                   FROM  logl.entries
--                  WHERE log = $1 AND user_time >= $3 AND user_time <= $4
--                    AND uuid > $2
--               ORDER BY (user_time, uuid) ASC
--                  LIMIT 256;
--  END IF;
--END;
--$$ LANGUAGE plpgsql ROWS 256;

