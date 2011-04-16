{-# LANGUAGE StandaloneDeriving
  #-}
module Language.LogL.UUID (v1, Data.UUID.UUID) where

import Data.ByteString.Char8

import Data.UUID
import System.UUID.V1

import Language.LogL.Pickle


instance Pickle UUID where
  i b = case (reads . unpack) b of [(uuid,"")] -> Just uuid
                                   _           -> Nothing
  o                          =  pack . show


{-| Generate a version 1 UUID.
 -}
v1                          ::  IO UUID
v1                           =  uuid

