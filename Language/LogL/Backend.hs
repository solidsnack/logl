{-# LANGUAGE OverloadedStrings
           , StandaloneDeriving
           , GADTs
           , RecordWildCards
           , TypeSynonymInstances
           , TemplateHaskell
           , TypeFamilies
           , FlexibleContexts
           , TupleSections
           , UndecidableInstances
  #-}
module Language.LogL.Backend where

import Prelude hiding (unlines)
import Control.Applicative
import Control.Arrow
import Control.Concurrent
import Control.Exception
import Control.Monad
import Data.ByteString.Char8 hiding (zip)
import Data.Digest.Murmur64
import Data.UUID
import Data.Either
import qualified Data.List as List
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Monoid
import Data.Time.Clock
import Data.Word
import System.IO.Error
import System.Posix.Types (CPid)

import qualified Database.PQ as PG

import Language.LogL.Syntax
import qualified Language.LogL.Pickle as Pickle
import qualified Language.LogL.Macros as Macros
import qualified Language.LogL.PG as PG


{-| Backends support a few tasks, allowing us to set, delete and retrieve logs
    and log entries. Backends are completely permissive as regards
    over-writing existing entries and so forth -- single-assignment invariants
    come from the interpreter layer, which manages the IDs.
 -}
data Task t where
  WriteLog                  ::  Log -> Task ()
  WriteEntry                ::  Entry -> Task ()
  WriteTombstone            ::  ID Log -> Task ()
  RetrieveSubtree           ::  ID Log -> ID Entry -> Task (Map Backedge Entry)

class Backend backend where
  type Info backend         ::  *
  type Spec backend         ::  *
  start                     ::  Spec backend -> IO backend
  stop                      ::  backend -> IO ()
  run :: (Monoid t) => backend -> Task t -> IO (Envelope backend t)

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
instance (Monoid t) => Monoid (Status t) where
  mempty                     =  ERROR
  mappend (OK a) (OK b)      =  OK (mappend a b)
  mappend ERROR other        =  other
  mappend other ERROR        =  other

ok                          ::  Status t -> Bool
ok ERROR                     =  False
ok (OK _)                    =  True

{-| Tuple of @(parent, child)@ IDs.
 -}
newtype Backedge             =  Backedge (ID Entry, ID Entry)
deriving instance Eq Backedge
deriving instance Ord Backedge
deriving instance Show Backedge


data Postgres                =  Postgres { conninfo :: PG.Conninfo,
                                           conn     :: PG.Connection,
                                           pid      :: CPid,
                                           lock     :: MVar ()       }
deriving instance Eq Postgres
instance Show Postgres where
  show Postgres{..} = "Postgres " ++ unpack (PG.renderconninfo conninfo)

data PostgresInfo            =  PostgresInfo CPid PG.Conninfo ByteString
deriving instance Eq PostgresInfo
deriving instance Show PostgresInfo

instance Backend Postgres where
  type Info Postgres         =  PostgresInfo
  type Spec Postgres         =  PG.Conninfo
  start conninfo             =  do
    conn                    <-  PG.connectdb (PG.renderconninfo conninfo)
    stat                    <-  PG.status conn
    when (stat /= PG.ConnectionOk) (error "Failed to connect.")
    encodingStat            <-  PG.setClientEncoding conn "UTF8"
    when (not encodingStat) (error "Failed to set encoding of PG connection.")
    guarded <- PG.guard "Connection setup." =<< PG.exec conn pgSetupCommands
    case guarded of
      Left b                ->  error (unpack b)
      Right result          ->  do pid <- PG.backendPID conn
                                   Postgres conninfo conn pid <$> newMVar ()
  stop Postgres{..}          =  PG.finish conn >> takeMVar lock
  run Postgres{..} task      =  (envelope . withMVar' lock) (dispatch task)
   where
    dispatch                ::  Task t -> IO (Info Postgres, Status t)
    dispatch task            =  case task of
      WriteLog _            ->  stat_only task
      WriteEntry _          ->  stat_only task
      WriteTombstone _      ->  stat_only task
      RetrieveSubtree _ _   ->  form_map task
    (text, params)           =  paramsForPGExec task
    execTask otype           =  do
      res                   <-  PG.execParams conn text params otype
      case res of
        Just res            ->  Right <$> return res
        Nothing             ->  Left <$> msg
--    res <- PG.execParamsInterruptible conn text params otype
--    case res of
--      PG.FailedSend       ->  Left <$> msg -- TODO -- Catch busy connection.
--      PG.FailedPolling    ->  Left <$> msg
--      PG.FailedResult     ->  Left <$> msg
--      PG.Received res     ->  Right <$> return res
     where
      msg                    =  maybe "" id <$> PG.errorMessage conn
    stat_only               ::  Task () -> IO (Info Postgres, Status ())
    stat_only task           =  do
      res                   <-  execTask PG.Text
      case res of Left err  ->  return (PostgresInfo pid conninfo err, ERROR)
                  Right _   ->  return (PostgresInfo pid conninfo "", OK ())
    form_map :: Task (Map Backedge Entry) ---------------------------------
             -> IO (Info Postgres, Status (Map Backedge Entry))
    form_map task            =  do
      res                   <-  execTask PG.Text
      case res of
        Left err            ->  return (PostgresInfo pid conninfo err, ERROR)
        Right result        ->  do
          (errors, okay)    <-  partitionEithers <$> PG.fromResult result
          let msg            =  unlines errors
          return (((PostgresInfo pid conninfo msg,) . OK . buildMap) okay)


pgSetupCommands             ::  ByteString
pgSetupCommands = $(Macros.text "./postgresql/stored_procedures.sql")

paramsForPGExec :: Task t ------------------------------------------------
                -> (ByteString, [Maybe (PG.Oid, ByteString, PG.Format)]) 
paramsForPGExec task         =  case task of
  WriteLog log              ->  (PG.call "logl.write_log" 4, PG.pqARGV log)
  WriteEntry entry          ->  (PG.call "logl.write_entry" 7, PG.pqARGV entry)
  WriteTombstone idL        ->  ( PG.call "logl.write_tombstone" 1,
                                  [Just (0, Pickle.o idL, PG.Text)] )
  RetrieveSubtree idL idE   ->  ( PG.call "logl.retrieve_subtree" 2,
                                  [ Just (0, Pickle.o idL, PG.Text),
                                    Just (0, Pickle.o idE, PG.Text) ] )

{-| The sharded backend multuplexes requests over a collection of backends by
    by hashing the log ID of the task. In the replication factor tuple, the
    second element is the number of shards to use for each request; the first
    element is the number that must return successfully for the sharded
    backend as a whole to return successfully.
 -}
data Sharded b               =  Sharded { n_of_m :: (Word8, Word8),
                                          shards :: [b]            }
deriving instance (Eq b) => Eq (Sharded b)
deriving instance (Show b) => Show (Sharded b)

instance (Backend b) => Backend (Sharded b) where
  type Info (Sharded b)      =  [Info b]
  type Spec (Sharded b)      =  ((Word8, Word8), [Spec b])
  start (n_of_m, specs)      =  Sharded n_of_m <$> mapM start specs
  stop Sharded{..}           =  mapM_ stop shards
  run Sharded{..} task       =  envelope $ do
    -- TODO -- Fix deadlock.
    results                 <-  mapM (flip run task) shardsToUse
    let opened               =  open <$> results
        infos                =  fst <$> opened
        merged               =  merge ERROR n (snd <$> opened)
    return (infos, merged)
   where
    (n, m)                   =  n_of_m
    key                      =  shardTask task
    fractional              ::  Double
    fractional               =  fromIntegral key
                             /  fromIntegral (maxBound :: Word64)
    index                   ::  Int
    index = floor (fractional * fromIntegral (List.length shards))
    (before, after) = (List.take index shards, List.drop index shards)
    shardsToUse              =  List.take (fromIntegral m) (after ++ before)
    open (Envelope _ _ info status) = (info, status)
    merge acc 0 []           =  acc
    merge _   _ []           =  ERROR
    merge acc 0 [ERROR]      =  acc
    merge acc 0 [OK t]       =  OK t `mappend` acc
    merge _   1 [ERROR]      =  ERROR
    merge acc 1 [OK t]       =  OK t `mappend` acc
    merge acc i (OK t:tail)  =  merge (OK t `mappend` acc) (i-1) tail
    merge acc i (ERROR:tail) =  merge acc i tail

shardTask                   ::  Task t -> Word64
shardTask task               =  shardLog $ case task of
  WriteLog (Log uuid _ _ _) ->  uuid
  WriteEntry Entry{..}      ->  log
  WriteTombstone log        ->  log
  RetrieveSubtree log _     ->  log

shardLog                    ::  ID Log -> Word64
shardLog (ID uuid)           =  (asWord64 . hash64) uuid


data Timeout b               =  Timeout { micros :: Word32, worker :: b }
deriving instance (Eq b) => Eq (Timeout b)
deriving instance (Show b) => Show (Timeout b)

data TimeoutInfo b           =  TIMEOUT | COMPLETED (Info b)
deriving instance (Eq (Info b)) => Eq (TimeoutInfo b)
deriving instance (Show (Info b)) => Show (TimeoutInfo b)

instance (Show (Info b), Backend b) => Backend (Timeout b) where
  type Info (Timeout b)      =  TimeoutInfo b
  type Spec (Timeout b)      =  (Word32, Spec b)
  start (micros, spec)       =  Timeout micros <$> start spec
  stop Timeout{..}           =  stop worker
  run Timeout{..} q          =  envelope $ do
    res                     <-  timeout micros (run worker q)
    return $ case res of
      Just (Envelope _ _ info val) -> (COMPLETED info, val)
      Nothing               ->  (TIMEOUT, ERROR)

timeout                     ::  Word32 -> IO t -> IO (Maybe t)
timeout w io                 =  do
  output                    <-  newEmptyMVar
  alarm                     <-  newEmptyMVar
  worker                    <-  forkIO $ do
                                  res    <- io
                                  ontime <- tryPutMVar output (Just res)
                                  when ontime (killThread =<< takeMVar alarm)
  (putMVar alarm =<<) . forkIO $ do
                          threadDelay (fromIntegral w)
                          overtime <- tryPutMVar output Nothing
                          when overtime (killThread worker)
  takeMVar output


withMVar'                   ::  MVar t' -> IO t -> IO t
withMVar' mvar               =  withMVar mvar . const


envelope :: (Backend i) => IO (Info i, Status t) -> IO (Envelope i t)
envelope io                  =  do
  start                     <-  getCurrentTime
  (msg, val)                <-  io
  stop                      <-  getCurrentTime
  return $ Envelope start stop msg val


buildMap                    ::  [Entry] -> Map Backedge Entry
buildMap                     =  List.foldl' insert' mempty
 where
  insert' map e@Entry{..}    =  Map.insert (Backedge (parent, uuid)) e map


secondM                     ::  (b -> IO c) -> (a, b) -> IO (a, c)
secondM f (a, b)             =  (a,) <$> f b

