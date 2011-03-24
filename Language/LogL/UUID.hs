{-# LANGUAGE StandaloneDeriving
  #-}
{-| With version 1 UUIDs, older ones have lesser numerical values than newer
    ones. When sequentially generating UUIDs for a bulk of log entries, we can
    rely on the fact that the first will sort earlier than the last, even if
    the two log entries have the same timestamp.
 -}
module Language.LogL.UUID (V1(), v1) where

import Data.ByteString.Char8
import Control.Applicative

import qualified Data.UUID
import qualified System.UUID.V1

import Language.LogL.Pickle


newtype V1                   =  V1 Data.UUID.UUID
deriving instance Eq V1
deriving instance Ord V1
deriving instance Show V1
instance Pickle V1 where
  i b = case (reads . unpack) b of [(uuid,"")] -> Just (V1 uuid)
                                   _           -> Nothing
  o (V1 uuid)                =  (pack . show) uuid

v1                          ::  IO V1
v1                           =  V1 <$> System.UUID.V1.uuid

