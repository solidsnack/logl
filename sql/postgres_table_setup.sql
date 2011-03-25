
DROP FUNCTION IF EXISTS "Log/L,v2011-03-25.setup"() CASCADE;
CREATE OR REPLACE FUNCTION "Log/L,v2011-03-25.setup"()
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
      , tag         bytea CHECK (length(tag) <= 128) NOT NULL ); 
    CREATE INDEX   "logs/tag"
              ON   "Log/L,v2011-03-25".logs (tag);
    CREATE INDEX   "logs/time"
              ON   "Log/L,v2011-03-25".logs (time);
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
    CREATE INDEX   "entries/time"
              ON   "Log/L,v2011-03-25".entries (time);
    RETURN NEXT    'Log/L,v2011-03-25.entries';
  END IF;
END;
$$ LANGUAGE plpgsql;
SELECT "Log/L,v2011-03-25.setup"();
