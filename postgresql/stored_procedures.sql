
--  idempotent setup
DROP FUNCTION IF EXISTS "Log/L,v2011-03-25"();
CREATE OR REPLACE FUNCTION "Log/L,v2011-03-25"()
  RETURNS SETOF text AS $$
BEGIN
  IF NOT EXISTS ( SELECT 1 FROM information_schema.schemata
                          WHERE schema_name = 'Log/L,v2011-03-25' )
  THEN
    CREATE SCHEMA  "Log/L,v2011-03-25";
    RETURN NEXT    'Log/L,v2011-03-25';
  END IF;
  IF NOT EXISTS ( SELECT 1 FROM information_schema.tables
                          WHERE table_schema = 'Log/L,v2011-03-25'
                            AND table_name = 'logs'                )
  THEN
    CREATE TABLE   "Log/L,v2011-03-25".logs
      ( uuid        uuid PRIMARY KEY
      , time        timestamp with time zone NOT NULL
      , destroy     boolean NOT NULL
      , tag         bytea CHECK (length(tag) <= 128) NOT NULL ); 
    CREATE INDEX   "logs/tag" ON "Log/L,v2011-03-25".logs (tag);
    CREATE INDEX   "logs/time" ON "Log/L,v2011-03-25".logs (time);
    RETURN NEXT    'Log/L,v2011-03-25.logs';
  END IF;
  IF NOT EXISTS ( SELECT 1 FROM information_schema.tables
                          WHERE table_schema = 'Log/L,v2011-03-25'
                            AND table_name = 'entries'             )
  THEN
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
  END IF;
  IF NOT EXISTS ( SELECT 1 FROM information_schema.tables
                          WHERE table_schema = 'Log/L,v2011-03-25'
                            AND table_name = 'entries_with_bool'   )
  THEN
    CREATE TABLE   "Log/L,v2011-03-25".entries_with_bool
      ( okay        boolean NOT NULL )
    INHERITS ("Log/L,v2011-03-25".entries);
    RETURN NEXT    'Log/L,v2011-03-25.entries_with_bool';
  END IF;
END;
$$ LANGUAGE plpgsql;
SELECT "Log/L,v2011-03-25"();

DROP FUNCTION IF EXISTS "Log/L,v2011-03-25".get_entry(id uuid);
CREATE OR REPLACE FUNCTION "Log/L,v2011-03-25".get_entry(id uuid)
  RETURNS SETOF "Log/L,v2011-03-25".entries_with_bool AS $$
BEGIN
--IF NOT EXISTS ( SELECT 1 FROM "Log/L,v2011-03-25".logs
--RETURN QUERY SELECT '00000000-0000-0000-0000-000000000000' :: uuid,
--                    '00000000-0000-0000-0000-000000000000' :: uuid, 
--                    '0001-01-01' :: timestamp with time zone,
--                    '0001-01-01' :: timestamp with time zone,
--                    '' :: bytea, false ;
  RETURN QUERY SELECT "Log/L,v2011-03-25".entries.*,
                      NOT("Log/L,v2011-03-25".logs.destroy)
                 FROM "Log/L,v2011-03-25".entries,
                      "Log/L,v2011-03-25".logs
                WHERE "Log/L,v2011-03-25".entries.uuid = id
                  AND "Log/L,v2011-03-25".logs.uuid =
                      "Log/L,v2011-03-25".entries.log;
END;
$$ LANGUAGE plpgsql;
