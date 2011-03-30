
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
      ( parent      uuid PRIMARY KEY,
        timestamp   timestamp with time zone NOT NULL );
    CREATE INDEX   "tombstone/timestamp" ON logl.tombstone (timestamp);
    RETURN NEXT    'logl.tombstone';
  EXCEPTION WHEN duplicate_table THEN END;
  BEGIN
    CREATE TABLE    logl.entry
      ( uuid        uuid PRIMARY KEY,
        parent      uuid NOT NULL,
        timestamp   timestamp with time zone NOT NULL,
        client_time timestamp with time zone NOT NULL,
        tag         bytea CHECK (length(tag) <= 128) NOT NULL,
        bytes       bytea NOT NULL                             );
    CREATE INDEX   "entry/timestamp" ON logl.entry (timestamp);
    CREATE INDEX   "entry/parent" ON logl.entry (parent);
    CREATE INDEX   "entry/parent,client_time"
              ON    logl.entry (parent,client_time);
    RETURN NEXT    'logl.entry';
  EXCEPTION WHEN duplicate_table THEN END;
  BEGIN
    CREATE VIEW     logl.entry_with_tombstone AS
         SELECT     entry.*, tombstone.timestamp AS tombstone
           FROM     logl.entry AS entry LEFT OUTER JOIN
                    logl.tombstone AS tombstone
             ON    (entry.parent = tombstone.parent)
          UNION
         SELECT     NULL, tombstone.parent, NULL, NULL, NULL, NULL,
                    tombstone.timestamp AS tombstone
           FROM     logl.tombstone AS tombstone;
    RETURN NEXT    'logl.entry_with_tombstone';
  EXCEPTION WHEN duplicate_table THEN END;
END;
$$ LANGUAGE plpgsql STRICT;
SELECT "logl#setup"();

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

--  LookupEntry               ::  ID Entry -> Task Entry
CREATE OR REPLACE FUNCTION logl.lookup_entry(uuid)
RETURNS SETOF logl.entry_with_tombstone AS $$
  WITH p AS (SELECT parent FROM logl.entry_with_tombstone WHERE uuid = $1)
  SELECT * FROM     logl.entry_with_tombstone
          WHERE    (uuid = $1 AND tombstone IS NULL)
             OR    (timestamp IS NULL AND parent = (SELECT * FROM p));
$$ LANGUAGE sql STRICT;

--  SearchEntries :: ID Log -> ID Entry -> (UTCTime, UTCTime) -> Tag
--                -> Task (Set Entry)
CREATE OR REPLACE FUNCTION logl.search_entries( uuid, uuid, bytea,
                                                timestamp with time zone,
                                                timestamp with time zone  )
RETURNS SETOF logl.entry_with_tombstone AS $$
  WITH              cursor_stamp AS ( SELECT client_time, uuid
                                        FROM logl.entry_with_tombstone
                                       WHERE uuid = $1                 )
  SELECT * FROM     logl.entry_with_tombstone
          WHERE     parent = $1 AND (timestamp IS NULL OR tombstone IS NULL)
            AND     client_time >= $4 AND client_time <= $5
            AND     POSITION($3 IN tag) = 1
            AND    (client_time, uuid) > (SELECT * FROM cursor_stamp)
       ORDER BY    (client_time, uuid) ASC
          LIMIT     256;
$$ LANGUAGE sql STRICT;

