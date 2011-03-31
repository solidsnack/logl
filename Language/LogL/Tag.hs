{-# LANGUAGE OverloadedStrings
           , StandaloneDeriving
  #-}
module Language.LogL.Tag (Tag(), max) where

import Data.ByteString.Char8
import Data.Monoid
import Data.String
import Data.Word
import Prelude hiding (length, take, max)

import qualified Data.ByteString.UTF8 as UTF8

import Language.LogL.Pickle


{-| A tag is a string of up to 128 bytes that clients supply to mark stored
    data.
 -}
newtype Tag                  =  Tag ByteString
deriving instance Eq Tag
deriving instance Ord Tag
deriving instance Show Tag
instance IsString Tag where
  fromString s               =  maybe (error msg) id (i b)
   where
    b                        =  UTF8.fromString s
    msg = "Tags may not be longer than " ++ show max ++ " bytes."
instance Monoid Tag where
  mempty                     =  ""
  Tag b `mappend` Tag b'     =  (Tag . take casted) (b `mappend` b')
instance Pickle Tag where
  i b | length b <= casted   =  Just (Tag b)
      | otherwise            =  Nothing
  o (Tag b)                  =  b

{-| The maximum length of a tag: 128 bytes. 
 -}
max                         ::  Word32
max                          =  128

casted                       =  fromIntegral max

