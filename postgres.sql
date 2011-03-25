
--  Tentative PostgreSQL schema with indices, fixed size fields and so on.

CREATE SCHEMA      "Log/L,v2011-03-25";

CREATE TABLE       "Log/L,v2011-03-25".logs
  ( uuid            uuid PRIMARY KEY
  , timestamp       timestamp with time zone NOT NULL
  , tag             bytea CHECK (length(tag) <= 128) NOT NULL ); 
CREATE INDEX       "logs/tag"
          ON       "Log/L,v2011-03-25".logs (tag);
CREATE INDEX       "logs/timestamp"
          ON       "Log/L,v2011-03-25".logs (timestamp);

CREATE TABLE       "Log/L,v2011-03-25".entries
  ( uuid            uuid NOT NULL
  , log             uuid NOT NULL
  , user_timestamp  timestamp with time zone NOT NULL
  , timestamp       timestamp with time zone NOT NULL
  , data            bytea NOT NULL
  ,                 PRIMARY KEY (uuid, log)           ); 
CREATE INDEX       "entries/log,user_timestamp"
          ON       "Log/L,v2011-03-25".entries (log, user_timestamp);
CREATE INDEX       "entries/timestamp"
          ON       "Log/L,v2011-03-25".entries (timestamp);

