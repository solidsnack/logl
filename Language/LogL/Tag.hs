{-# LANGUAGE OverloadedStrings
           , StandaloneDeriving
  #-}
module Language.LogL.Tag (Tag(), max) where

import Data.Word
import Data.ByteString.Char8
import Data.String

import Language.LogL.Pickle


{-| A tag is a string of up to 128 bytes that clients supply to mark stored
    data.
 -}
newtype Tag                  =  Tag ByteString
deriving instance Eq Tag
deriving instance Ord Tag
deriving instance Show Tag
deriving instance IsString Tag
instance Monoid Tag where
  mempty                     =  ""
  Tag b `mappend` Tag b'     =  take 128 (b `mappend` b')
instance Pickle Tag where
  i b | length b <= max      =  Just (Tag b)
      | otherwise            =  Nothing
  o (Tag b)                  =  b

{-| The maximum length of a tag: 128 bytes. 
 -}
max                         ::  Word32
max                          =  128

