
--  SetLog
INSERT INTO logs (uuid, time, destroy, tag) VALUES (?, ?, FALSE, ?); 

--  SetEntry
INSERT INTO entries (uuid, log, user_time, time, data) VALUES (?, ?, ?, ?, ?); 

--  GetLog
SELECT uuid, time, tag FROM logs WHERE uuid = ?;

--  GetEntry
SELECT uuid, log, user_time, data FROM entries WHERE uuid = ?;

--  DeleteLog
DELETE FROM logs WHERE uuid = ?;

--  DeleteEntry
DELETE FROM entries WHERE uuid = ?;

--  EntryTimeRangeSearch
SELECT uuid, log, user_time, data FROM entries
                                 WHERE user_time >= ? AND user_time <= ?;

--  LogTagSearch
SELECT uuid, time, tag FROM logs WHERE tag = ?;

