{-| Parsing and pretty printing for the YAML request and response format.
 -}
module Language.LogL.YAML where

import Control.Applicative
import Data.ByteString

import Control.Failure
import Data.Object hiding ( lookupMapping, lookupObject
                          , lookupScalar, lookupSequence )
import qualified Data.Object
import Data.Object.Yaml

import Language.LogL.Control.Failure.Either
import qualified Language.LogL.Pickle as Pickle
import Language.LogL.Syntax


--data LogL t where
--  Alloc             ::  (UTCTime, Tag) -> LogL (ID Log)
--  Append            ::  ID Log -> ID Entry -> Message -> LogL (ID Entry)
--  Free              ::  ID Log -> LogL ()
--  Forest            ::  ID Log -> ID Entry -> LogL [Tree Entry]


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

