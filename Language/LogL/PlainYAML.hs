{-# LANGUAGE OverloadedStrings
           , RecordWildCards
  #-}
{-| Safe YAML serialization; escapes all control characters and high bytes.
    Uses the inline format for YAML, so there are no newlines, either.
    Suitable for YAML that is to be included in syslog messages or text areas,
    for example.
 -}
module Language.LogL.PlainYAML where

import Control.Applicative
import Data.ByteString.Char8
import qualified Data.List as List
import Data.Monoid

import Blaze.ByteString.Builder
import Data.Object.Yaml (YamlObject, YamlScalar(..))
import Data.Object


encode                      ::  YamlObject -> Builder
encode (Mapping pairs)       =  compound "{ " (encodeKV <$> pairs) " }"
encode (Sequence items)      =  compound "[ " (encode   <$> items) " ]"
encode (Scalar s)            =  fromByteString (escape s)

escape                      ::  YamlScalar -> ByteString
escape YamlScalar{..}        =  error "ah ah ah ah ah"

encodeKV (k, v) = fromByteString (escape k `mappend` ": ") `mappend` encode v

compound delimiterLeft vals delimiterRight =
  delimiterLeft' `mappend` mconcat vals' `mappend` delimiterRight'
 where
  delimiterLeft'             =  fromByteString delimiterLeft
  vals'                      =  List.intersperse (fromByteString ", ") vals
  delimiterRight'            =  fromByteString delimiterRight

