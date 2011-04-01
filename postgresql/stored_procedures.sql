
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
    CREATE TABLE    logl.entry
      ( uuid        uuid PRIMARY KEY,
        timestamp   timestamp with time zone NOT NULL,
        client_time timestamp with time zone NOT NULL,
        tag         bytea CHECK (length(tag) <= 128) NOT NULL,
        bytes       bytea NOT NULL                             );
    CREATE INDEX   "entry/timestamp" ON logl.entry (timestamp);
    CREATE INDEX   "entry/tag" ON logl.entry (tag);
    CREATE INDEX   "entry/client_time" ON logl.entry (client_time);
    COMMENT ON TABLE logl.entry IS 'Basic data for individual entries.';
    RETURN NEXT    'logl.entry';
  EXCEPTION WHEN duplicate_table THEN END;
  BEGIN
    CREATE TABLE    logl.tombstone
      ( entry       uuid PRIMARY KEY,
        timestamp   timestamp with time zone NOT NULL );
    CREATE INDEX   "tombstone/timestamp" ON logl.tombstone (timestamp);
    COMMENT ON TABLE logl.tombstone IS
      'Trees below which data is no longer live.';
    RETURN NEXT    'logl.tombstone';
  EXCEPTION WHEN duplicate_table THEN END;
  BEGIN
    CREATE TABLE    logl.child_to_parent
      ( child       uuid PRIMARY KEY REFERENCES logl.entry ON DELETE CASCADE,
        parent      uuid NOT NULL                                             );
    CREATE INDEX   "child_to_parent/parent" ON logl.child_to_parent (parent);
    COMMENT ON TABLE logl.child_to_parent IS
      'Relates a child to its immediate parent.';
    RETURN NEXT    'logl.entry';
  EXCEPTION WHEN duplicate_table THEN END;
  BEGIN
    -- We already have an index for this is in child_to_parent so no need to
    -- create a fresh table.
    CREATE VIEW     logl.parent_to_children AS
         SELECT     parent, child
           FROM     logl.child_to_parent;
    RETURN NEXT    'logl.parent_to_children';
  EXCEPTION WHEN duplicate_table THEN END;
END;
$$ LANGUAGE plpgsql STRICT;
SELECT "logl#setup"();

--  WriteEntry              ::  !Entry -> Task ()
CREATE OR REPLACE FUNCTION logl.write_entry( uuid, uuid,
                                             timestamp with time zone,
                                             timestamp with time zone,
                                             bytea, bytea              )
RETURNS VOID AS $$
BEGIN
  INSERT INTO       logl.entry                    VALUES ($1, $3, $4, $5, $6);
  INSERT INTO       logl.child_to_parent          VALUES ($1, $2);
EXCEPTION WHEN unique_violation THEN END;
$$ LANGUAGE plpgsql STRICT;

--  WriteTombstone          ::  !ID -> Task ()
CREATE OR REPLACE FUNCTION logl.write_tombstone(uuid, timestamp with time zone)
RETURNS VOID AS $$
BEGIN
  INSERT INTO       logl.tombstone                VALUES ($1, $2);
EXCEPTION WHEN unique_violation THEN END;
$$ LANGUAGE plpgsql STRICT;

--  SomeLeaves              ::  !ID -> Task Children
CREATE OR REPLACE FUNCTION logl.some_leaves(uuid)
RETURNS SETOF logl.entry AS $$
DECLARE
  root uuid;
  roots uuid[] = ARRAY[$1];
  next_roots uuid[];
  leaves uuid[];
BEGIN -- Does not handle cycles at all.
  LOOP
    IF roots = ARRAY[] THEN
      EXIT;
    END IF;
    next_roots := ARRAY[];
    FOR root IN SELECT * FROM unnest(roots) LOOP
      IF NOT EXISTS (SELECT 1 FROM logl.tombstones WHERE entry = root) THEN
        SELECT ARRAY( SELECT child FROM logl.parent_to_children
                                  WHERE parent = root           )
          INTO leaves;
        IF leaves = ARRAY[] THEN
          RETURN QUERY SELECT * FROM logl.entries WHERE uuid = root;
        ELSE
          next_roots := next_roots || leaves;
        END IF;
      END IF;
    END LOOP;
    roots := next_roots;
  END LOOP;
END;
$$ LANGUAGE plpgsql STRICT;

--  ParentsBelow            ::  !ID -> !ID -> Task Parents
--CREATE OR REPLACE FUNCTION logl.ParentsBelow(uuid, uuid)
--RETURNS SETOF logl.entry_with_tombstone AS $$
--  WITH              cursor_stamp AS ( SELECT client_time, uuid
--                                        FROM logl.entry_with_tombstone
--                                       WHERE uuid = $1                 )
--  SELECT * FROM     logl.entry_with_tombstone
--          WHERE     parent = $1 AND (timestamp IS NULL OR tombstone IS NULL)
--            AND     client_time >= $4 AND client_time <= $5
--            AND     POSITION($3 IN tag) = 1
--            AND    (client_time, uuid) > (SELECT * FROM cursor_stamp)
--       ORDER BY    (client_time, uuid) ASC
--          LIMIT     256;
--$$ LANGUAGE sql STRICT;

