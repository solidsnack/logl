
--  Ensure plpgsql is available. Drawn from:
--    http://wiki.postgresql.org/wiki/CREATE_OR_REPLACE_LANGUAGE
CREATE OR REPLACE FUNCTION make_plpgsql() RETURNS VOID
  AS $$ CREATE LANGUAGE plpgsql; $$ LANGUAGE sql;
SELECT CASE WHEN EXISTS ( SELECT 1 FROM pg_catalog.pg_language
                                   WHERE lanname='plpgsql'     ) THEN NULL
            ELSE make_plpgsql() END;
DROP FUNCTION make_plpgsql();

--  Idempotent setup of schema, tables and types.
CREATE OR REPLACE FUNCTION "logl#setup"()
  RETURNS SETOF text AS $$
BEGIN
  BEGIN
    CREATE SCHEMA   logl;
    COMMENT ON SCHEMA logl IS 'v2011-03-29';
    RETURN NEXT    'logl';
  EXCEPTION WHEN duplicate_schema THEN END;
  BEGIN
    CREATE TABLE    logl.tombstone
      ( log         uuid PRIMARY KEY,
        timestamp   timestamp with time zone NOT NULL );
    CREATE INDEX   "tombstone/timestamp" ON logl.tombstone (timestamp);
    RETURN NEXT    'logl.tombstone';
  EXCEPTION WHEN duplicate_table THEN END;
  BEGIN
    CREATE TABLE    logl.log
      ( uuid        uuid PRIMARY KEY,
        timestamp   timestamp with time zone NOT NULL,
        tag         bytea CHECK (length(tag) <= 128) NOT NULL );
    CREATE INDEX   "log/timestamp" ON logl.log (timestamp);
    CREATE INDEX   "log/tag" ON logl.log (tag);
    RETURN NEXT    'logl.log';
  EXCEPTION WHEN duplicate_table THEN END;
  BEGIN
    CREATE VIEW     logl.log_with_tombstone AS
         SELECT     log.*, tombstone.timestamp AS tombstone
           FROM     logl.log AS log LEFT OUTER JOIN
             	    logl.tombstone AS tombstone
             ON    (log.uuid = tombstone.log)
          UNION
         SELECT     tombstone.log, NULL, NULL,
                    tombstone.timestamp AS tombstone
           FROM     logl.tombstone AS tombstone;
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
    CREATE INDEX   "entry/timestamp" ON logl.entry (timestamp);
    CREATE INDEX   "entry/log,client_time"
              ON    logl.entry (log,client_time);
    CREATE INDEX   "entry/log" ON  logl.entry (log);
    RETURN NEXT    'logl.entry';
  EXCEPTION WHEN duplicate_table THEN END;
  BEGIN
    CREATE VIEW     logl.entry_with_tombstone AS
         SELECT     entry.*, tombstone.timestamp AS tombstone
           FROM     logl.entry AS entry LEFT OUTER JOIN
                    logl.tombstone AS tombstone
             ON    (entry.log = tombstone.log)
          UNION
         SELECT     NULL, tombstone.log, NULL, NULL, NULL, NULL,
                    tombstone.timestamp AS tombstone
           FROM     logl.tombstone AS tombstone;
    RETURN NEXT    'logl.entry_with_tombstone';
  EXCEPTION WHEN duplicate_table THEN END;
END;
$$ LANGUAGE plpgsql STRICT;
SELECT "logl#setup"();

--  WriteLog                  ::  Log -> Task ()
CREATE OR REPLACE FUNCTION logl.write_log( uuid, timestamp with time zone,
                                           bytea )
RETURNS VOID AS $$
  INSERT INTO       logl.log
       VALUES      ($1, $2, $3);
$$ LANGUAGE sql STRICT;

--  WriteEntry                ::  Entry -> Task ()
CREATE OR REPLACE FUNCTION logl.write_entry( uuid, uuid,
                                             timestamp with time zone,
                                             timestamp with time zone,
                                             bytea, bytea              )
RETURNS VOID AS $$
  INSERT INTO       logl.entry
       VALUES      ($1, $2, $3, $4, $5, $6);
$$ LANGUAGE sql STRICT;

--  WriteTombstone            ::  ID Log -> Task ()
CREATE OR REPLACE FUNCTION logl.write_tombstone(uuid, timestamp with time zone)
RETURNS VOID AS $$
  INSERT INTO       logl.tombstone
       VALUES      ($1, $2);
$$ LANGUAGE sql STRICT;

--  LookupLog                 ::  ID Log -> Task Log
CREATE OR REPLACE FUNCTION logl.lookup_log(uuid)
RETURNS SETOF logl.log_with_tombstone AS $$
  SELECT * FROM     logl.log_with_tombstone
          WHERE     uuid = $1 AND (timestamp IS NULL OR tombstone IS NULL);
$$ LANGUAGE sql STRICT;

--  LookupEntry               ::  ID Entry -> Task Entry
CREATE OR REPLACE FUNCTION logl.lookup_entry(uuid)
RETURNS SETOF logl.entry_with_tombstone AS $$
DECLARE
  res logl.entry_with_tombstone;
BEGIN
  SELECT * INTO res
    FROM logl.entry_with_tombstone
   WHERE uuid = $1;
  IF FOUND THEN
    IF res.tombstone IS NOT NULL THEN
      SELECT NULL, res.log, NULL, NULL, NULL, NULL, res.tombstone INTO res;
    END IF;
    RETURN NEXT res;
  END IF;
END;
$$ LANGUAGE plpgsql STRICT;

--  SearchEntries :: ID Log -> (UTCTime, UTCTime) -> ID Entry -> Task [Entry]
CREATE OR REPLACE FUNCTION logl.search_entries( uuid, uuid,
                                                timestamp with time zone,
                                                timestamp with time zone  )
RETURNS SETOF logl.entry_with_tombstone AS $$
  SELECT * FROM     logl.entry_with_tombstone
          WHERE     log = $1 AND (timestamp IS NULL OR tombstone IS NULL)
            AND     client_time >= $3 AND client_time <= $4
            AND    (client_time, uuid) > ( SELECT client_time, uuid
                                             FROM logl.entry_with_tombstone
                                            WHERE uuid = $1                 )
       ORDER BY    (client_time, uuid) ASC
          LIMIT     256;
$$ LANGUAGE sql STRICT;


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

