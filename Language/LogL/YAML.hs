{-# LANGUAGE OverloadedStrings
           , NoMonomorphismRestriction
           , FlexibleContexts
           , RecordWildCards
           , RankNTypes
           , GADTs
  #-}
{-| Parsing and pretty printing for the YAML request and response format.
 -}
module Language.LogL.YAML where

import Control.Applicative
import Data.Tree
import Data.ByteString hiding (append, reverse)

import Control.Failure
import Data.Object hiding ( lookupMapping, lookupObject
                          , lookupScalar, lookupSequence )
import qualified Data.Object
import Data.Object.Yaml
import Text.Libyaml

import qualified Language.LogL.Pickle as Pickle
import Language.LogL.Syntax
import Language.LogL.Backend


class ToYAML t where
  oYAML                     ::  t -> YamlObject

data Request where
  Request                   ::  forall t. LogL t -> Request

request :: ( Alternative m, Applicative m, ----------------------------------
             Failure ObjectExtractError m, Functor m )
        => [(YamlScalar, YamlObject)] -> m (Maybe Request)
request m                    =  (Request <$>) <$> alloc m
                            <|> (Request <$>) <$> append m
                            <|> (Request <$>) <$> free m
                            <|> (Request <$>) <$> forest m

alloc :: (Applicative m, Failure ObjectExtractError m, Functor m) -----
      => [(YamlScalar, YamlObject)] -> m (Maybe (LogL (ID Log)))
alloc m                      =  do
  m'                        <-  lookupMapping "alloc" m
  time                      <-  lookupScalar "time" m'
  tag                       <-  lookupScalar "tag" m'
  pure (Alloc <$> Pickle.i time <*> Pickle.i tag)

append :: ( Alternative m, Applicative m, -----------------------------------
            Failure ObjectExtractError m, Functor m )
       => [(YamlScalar, YamlObject)] -> m (Maybe (LogL [Tree (ID Entry)]))
append m                     =  do
  m'                        <-  lookupMapping "append" m
  log                       <-  lookupScalar "log" m'
  parent                    <-  lookupScalar "below" m'
  messages                  <-  messages m
  pure (Append <$> Pickle.i log <*> Pickle.i parent <*> messages)

free :: (Applicative m, Failure ObjectExtractError m, Functor m) -----
     => [(YamlScalar, YamlObject)] -> m (Maybe (LogL ()))
free m                       =  do
 log                        <-  lookupScalar "free" m
 pure (Free <$> Pickle.i log)

forest :: (Applicative m, Failure ObjectExtractError m, Functor m) -----
       => [(YamlScalar, YamlObject)] -> m (Maybe (LogL ([Tree Entry])))
forest m                     =  do
  m'                        <-  lookupMapping "forest" m
  log                       <-  lookupScalar "log" m'
  parent                    <-  lookupScalar "below" m'
  pure (Forest <$> Pickle.i log <*> Pickle.i parent)


message :: ( Alternative m, Applicative m, ----------------------------------
             Failure ObjectExtractError m, Functor m )
        => [(YamlScalar, YamlObject)] -> m (Maybe (Tree Message))
message m                    =  do
  tag                       <-  lookupScalar "tag" m
  time                      <-  lookupScalar "time" m
  bytes                     <-  lookupScalar "bytes" m
  messages                  <-  messages m
  pure $ do
    message <- Message <$> Pickle.i tag <*> Pickle.i time <*> Pickle.i bytes
    Node message <$> messages

messages :: ( Alternative m, Applicative m, ---------------------------------
              Failure ObjectExtractError m, Functor m )
         => [(YamlScalar, YamlObject)] -> m (Maybe [Tree Message])
messages m                   =  do
  messages                  <-  lookupSequence "messages" m <|> pure []
  maps                      <-  sequence (fromMapping <$> messages)
  trees                     <-  sequence (message <$> maps)
  (pure . sequence) trees


lookupMapping :: (Failure ObjectExtractError m, Functor m) -------------------
              => ByteString -> [(YamlScalar, YamlObject)]
              -> m [(YamlScalar, YamlObject)]
lookupMapping b              =  Data.Object.lookupMapping (toYamlScalar b)

lookupObject :: (Failure ObjectExtractError m, Functor m) -------------------
             => ByteString -> [(YamlScalar, YamlObject)]
             -> m YamlObject
lookupObject b               =  Data.Object.lookupObject (toYamlScalar b)

lookupScalar :: (Failure ObjectExtractError m, Functor m) -------------------
             => ByteString -> [(YamlScalar, YamlObject)]
             -> m ByteString
lookupScalar b m             =  fromYamlScalar
                            <$> Data.Object.lookupScalar (toYamlScalar b) m

lookupSequence :: (Failure ObjectExtractError m, Functor m) -------------------
               => ByteString -> [(YamlScalar, YamlObject)]
               -> m [YamlObject]
lookupSequence b             =  Data.Object.lookupSequence (toYamlScalar b)


renderKV                     =  Data.Object.Yaml.encode . worker []
 where
  worker stuff []            =  (toYamlObject . Mapping . reverse) stuff
  worker stuff ((a,b):tail)  =  worker (pair:stuff) tail
   where
    pair                     =  (y a, Data.Object.Scalar (y b))
  y bytes = YamlScalar { value=bytes, tag=NoTag, style=Any }

statYAML                    ::  LogL t -> Status t -> YamlObject
statYAML (Alloc _ _) (OK u)  =  p "alloc"              $ sy (Pickle.o u)
statYAML (Append _ _ _) (OK t) = p "append"            $ roseToYAML t
statYAML (Free _) (OK ())    =  p "free"               $ sy "true"
statYAML (Forest _ _) (OK t) =  p "forest"             $ entriesToYAML t
statYAML (Alloc _ _) ERROR   =  p "alloc"   (p "error" $ sy "Bad allocation.")
statYAML (Append _ _ _) ERROR = p "append"  (p "error" $ sy "Bad append.")
statYAML (Free _) ERROR      =  p "free"    (p "error" $ sy "Bad free.")
statYAML (Forest _ _) ERROR  =  p "forest"  (p "error" $ sy "Bad forest.")

p                           ::  ByteString -> YamlObject -> YamlObject
p k v                        =  Data.Object.Mapping [(s k, v)]

s                           ::  ByteString -> YamlScalar
s bytes                      =  YamlScalar {value=bytes, tag=NoTag, style=Any}

sy                          ::  ByteString -> YamlObject
sy                           =  Data.Object.Scalar . s


roseToYAML                  ::  (Pickle.Pickle t) => Forest t -> YamlObject
roseToYAML forest            =  Data.Object.Mapping (f <$> forest)
 where
  f Node{..} = ((s . Pickle.o) rootLabel, roseToYAML subForest)

entriesToYAML forest         =  Data.Object.Sequence (f <$> forest)
 where
  f Node{..}                 =  Data.Object.Mapping keys
   where
    Entry{..}                =  rootLabel
    syP                      =  sy . Pickle.o
    keys                     =  [ (s "uuid",     syP uuid),
                                  (s "received", syP timestamp),
                                  (s "tag",      syP tag),
                                  (s "time",     syP client_time),
                                  (s "bytes",    sy bytes),
                                  (s "entries",  entriesToYAML subForest) ]

