{-# LANGUAGE OverloadedStrings
           , StandaloneDeriving
           , GADTs
           , RecordWildCards
           , TypeSynonymInstances
           , TemplateHaskell
  #-}
module Language.LogL.Backend where

import Control.Applicative
import Control.Concurrent
import Control.Exception
import Control.Monad
import Data.ByteString.Char8
import Data.Tree (Tree)
import qualified Data.Tree as Tree
import qualified Data.List as List
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Monoid
import Data.Time.Clock
import Data.Word
import System.IO.Error

import qualified Database.PQ as PG

import Language.LogL.Syntax
import qualified Language.LogL.Pickle as Pickle
import qualified Language.LogL.Macros as Macros
import qualified Language.LogL.PGConninfo as PG


{-| Backends support a few tasks, allowing us to set, delete and retrieve logs
    and log entries. Backends are completely permissive as regards
    over-writing existing entries and so forth -- single-assignment invariants
    come from the interpreter layer, which manages the IDs.
 -}
data Task t where
  WriteLog                  ::  Log -> Task ()
  WriteEntry                ::  Entry -> Task ()
  WriteTombstone            ::  ID Log -> Task ()
  RetrieveSubtree           ::  ID Log -> ID Entry -> Task (Tree Entry)

taskName                    ::  Task t -> ByteString
taskName (WriteLog _)        =  "WriteLog"
taskName (WriteEntry _)      =  "WriteEntry"
taskName (WriteTombstone _)  =  "WriteTombstone"
taskName (RetrieveSubtree _ _) = "RetrieveSubtree"

data Envelope b t where
  Envelope :: (Backend b) ---------------------------------------------
           => !UTCTime -> !UTCTime -> Info b -> Status t -> Envelope b t
deriving instance (Eq t, Eq (Info b)) => Eq (Envelope b t)
deriving instance (Show t, Show (Info b)) => Show (Envelope b t)

{-| A search may fail or it may return a result.
 -}
data Status t                =  OK !t | ERROR
deriving instance (Eq t) => Eq (Status t)
deriving instance (Show t) => Show (Status t)

class Backend backend where
  type Info backend         ::  *
  type Spec backend         ::  *
  start                     ::  Spec backend -> IO backend
  stop                      ::  backend -> IO ()
  run                       ::  backend -> Task t -> IO (Envelope backend t)


data Postgres                =  Postgres { conninfo :: PG.Conninfo,
                                           conn :: PG.Connection,
                                           lock :: MVar ()          }
deriving instance Eq Postgres
instance Show Postgres where
  show Postgres{..} = "Postgres " ++ unpack (PG.renderconninfo conninfo)
instance Backend Postgres where
  type Info Postgres         =  ByteString
  type Spec Postgres         =  PG.Conninfo
  start conninfo             =  do
    conn                    <-  PG.connectdb (PG.renderconninfo conninfo)
    stat                    <-  PG.setClientEncoding conn "UTF8"
    when (not stat) (error "Failed to set encoding of PG connection.")
    guarded <- pgGuard "Connection setup." =<< PG.exec conn pgSetupCommands
    case guarded of
      Left b                ->  error (unpack b)
      Right result          ->  Postgres conninfo conn <$> newMVar ()
  stop Postgres{..}          =  PG.finish conn >> takeMVar lock
  run Postgres{..} task      =  envelope $ dispatch task
   where
    dispatch                ::  Task t -> IO (Info Postgres, Status t)
    dispatch task            =  case task of
      WriteLog _            ->  stat_only task
      WriteEntry _          ->  stat_only task
      WriteTombstone _      ->  stat_only task
      RetrieveSubtree _ _   ->  form_tree task
    (text, params)           =  paramsForPGExec task
    execTask                 =  do
      res                   <-  PG.execParams conn text params PG.Binary
      pgGuard (mconcat ["Task: ", taskName task, "."]) res
    stat_only               ::  Task () -> IO (Info Postgres, Status ())
    stat_only task           =  do
      res                   <-  execTask
      case res of Left err  ->  return (err, ERROR)
                  Right _   ->  return ("", OK ())
    form_tree :: Task (Tree Entry) -> IO (Info Postgres, Status (Tree Entry))
    form_tree task           =  do
      res                   <-  execTask
      case res of Left err  ->  return (err, ERROR)
                  Right _   ->  return ("", OK (Tree.Node shim []))
     where
      shim                   =  Entry "00000000-0000-0000-0000-000000000000"
                                      "00000000-0000-0000-0000-000000000000"
                                      "00000000-0000-0000-0000-000000000000" 
                                      "2011-04-07 03:05:49.105519 UTC"
                                      "2011-04-07 03:05:49.105519 UTC"
                                      "shim"
                                      ""

--  Example of using libpq:
--  conn <- connectdb ""
--  trace conn System.IO.stderr
--  Just res <- exec conn "SELECT * FROM logl.write_log('00000000-0000-0000-0000-000000000000','2011-04-07 03:05:49.105519 UTC','2011-04-07 03:05:49.105519 UTC','');"
--  Just res <- execParams conn "SELECT * FROM logl.log WHERE uuid = $1;" [Just (0, "00000000-0000-0000-0000-000000000000", Text)] Text
--  resultStatus res
--  getvalue res (toRow 0) (toColumn 0)

pgGuard :: ByteString -------------------------------------------------
        -> Maybe PG.Result -> IO (Either ByteString PG.Result)
pgGuard rem Nothing = return . Left $ mappend "No response from server. " rem
pgGuard rem (Just result)    =  do
  stat                      <-  PG.resultStatus result
  msg                       <-  maybe "" id <$> PG.resultErrorMessage result
  let errmsg                 =  mconcat ["Query failed. ", rem, " ", msg]
  return $ case stat of
    PG.BadResponse          ->  Left errmsg
    PG.FatalError           ->  Left errmsg
    _                       ->  Right result

pgSetupCommands             ::  ByteString
pgSetupCommands = $(Macros.text "./postgresql/stored_procedures.sql")

paramsForPGExec :: Task t ------------------------------------------------
                -> (ByteString, [Maybe (PG.Oid, ByteString, PG.Format)]) 
paramsForPGExec task         =  case task of
  WriteLog (Log i t ct tag) ->  ( call "logl.write_log" 4,
                                  [ Just (0, Pickle.o i, PG.Text),
                                    Just (0, Pickle.o t, PG.Text),
                                    Just (0, Pickle.o ct, PG.Text),
                                    Just (0, Pickle.o tag, PG.Binary) ] )
  WriteEntry Entry{..}      ->  ( call "logl.write_entry" 7,
                                  [ Just (0, Pickle.o log, PG.Text),
                                    Just (0, Pickle.o parent, PG.Text),
                                    Just (0, Pickle.o uuid, PG.Text),
                                    Just (0, Pickle.o timestamp, PG.Text),
                                    Just (0, Pickle.o client_time, PG.Text),
                                    Just (0, Pickle.o tag, PG.Binary),
                                    Just (0, bytes, PG.Binary),
                                    Just (0, Pickle.o parent, PG.Text)      ] )
  WriteTombstone idL        ->  ( call "logl.write_tombstone" 1,
                                  [Just (0, Pickle.o idL, PG.Text)] )
  RetrieveSubtree idL idE   ->  ( call "logl.retrieve_subtree" 2,
                                  [ Just (0, Pickle.o idL, PG.Text),
                                    Just (0, Pickle.o idE, PG.Text) ] )

 where
  call                      ::  ByteString -> Word -> ByteString
  call name arity            =  mconcat
    [ "SELECT * FROM ", name, "(", 
      intercalate ", " ((cons '$' . pack . show) <$> [1..arity]),
      ");"                                                       ]

--data SQLite
--  = SQLite { db :: SQLite3.Database, path :: String, lock :: MVar () }
--instance Backend SQLite where
--  type Info SQLite           =  ByteString
--  type Spec SQLite           =  String
--  start path                 =  do
--    db                      <-  SQLite3.open path
--    mapM_ (sqlite_one_step db) $(Macros.blocks_list "./sqlite/ddl.sql")
--    SQLite db path <$> newMVar ()
--  stop SQLite{..}            =  withMVar' lock (SQLite3.close db)
--  run SQLite{..} queries     =  withMVar' lock . sequence
--                             $  (envelope . run_once) <$> queries
--   where
--    check_exc exc = (guard . isUserError) exc >> Just (ioeGetErrorString exc)
--    run_once q               =  do
--      result                <-  tryJust check_exc $ do
--                                    stmt <- SQLite3.prepare db ""
--                                    return ERROR
--      return $ case result of
--        Left msg            ->  (Pickle.o msg, ERROR)
--        Right val           ->  ("", val)

--sqlite_one_step             ::  SQLite3.Database -> String -> IO ()
--sqlite_one_step db str       =  do
--  stmt                      <-  SQLite3.prepare db str
--  _                         <-  SQLite3.step stmt
--  SQLite3.finalize stmt


--data Timeout i               =  Timeout { wait :: Word16, worker :: i }
--data TimeoutInfo i           =  TIMEOUT | (Backend i) => COMPLETED (Info i)
--instance (Backend i) => Backend (Timeout i) where
--  type Info (Timeout i)      =  TimeoutInfo i
--  type Spec (Timeout i)      =  (Word16, Spec i)
--  start (wait, spec)         =  Timeout wait <$> start spec
--  stop Timeout{..}           =  stop worker
--  run Timeout{..} q          =  envelope $ do
--    res                     <-  timeout wait (run worker q)
--    return $ case res of
--      Just (Envelope _ _ info val) -> (COMPLETED info, val)
--      Nothing               ->  (TIMEOUT, ERROR)

--timeout                     ::  Word16 -> IO t -> IO (Maybe t)
--timeout w io                 =  do
--  output                    <-  newEmptyMVar
--  alarm                     <-  newEmptyMVar
--  worker                    <-  forkIO $ do
--                                  res    <- io
--                                  ontime <- tryPutMVar output (Just res)
--                                  when ontime (killThread =<< takeMVar alarm)
--  (putMVar alarm =<<) . forkIO $ do
--                          threadDelay (fromIntegral w)
--                          overtime <- tryPutMVar output Nothing
--                          when overtime (killThread worker)
--  takeMVar output


withMVar'                   ::  MVar t' -> IO t -> IO t
withMVar' mvar               =  withMVar mvar . const


envelope :: (Backend i) => IO (Info i, Status t) -> IO (Envelope i t)
envelope io                  =  do
  start                     <-  getCurrentTime
  (msg, val)                <-  io
  stop                      <-  getCurrentTime
  return $ Envelope start stop msg val

