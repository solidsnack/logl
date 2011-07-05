{-# LANGUAGE OverloadedStrings
           , RecordWildCards
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

