
CREATE TABLE IF NOT EXISTS logs
  ( uuid            CHAR(36) PRIMARY KEY
  , time            CHAR(40) NOT NULL
  , destroy         BOOLEAN
  , tag             BLOB CHECK(length(tag) <= 128) NOT NULL ); 

CREATE INDEX IF NOT EXISTS "logs/time" ON logs (time);


CREATE TABLE IF NOT EXISTS entries
  ( uuid            CHAR(36)
  , log             CHAR(36)
  , user_time       CHAR(40) NOT NULL
  , time            CHAR(40) NOT NULL
  , data            BLOB NOT NULL
  ,                 PRIMARY KEY (uuid, log) ); 

CREATE INDEX IF NOT EXISTS "entries/time" ON entries (time);

CREATE INDEX IF NOT EXISTS "entries/log,user_time" ON entries (log,user_time);

