
CREATE TABLE IF NOT EXISTS logs
  ( uuid            CHAR(36) PRIMARY KEY
  , timestamp       CHAR(40) NOT NULL
  , destroy         BOOLEAN
  , tag             BLOB CHECK(length(tag) <= 128) NOT NULL ); 

CREATE TABLE IF NOT EXISTS entries
  ( uuid            CHAR(36)
  , log             CHAR(36)
  , user_timestamp  CHAR(40) NOT NULL
  , timestamp       CHAR(40) NOT NULL
  , data            BLOB NOT NULL
  ,                 PRIMARY KEY (uuid, log) ); 

