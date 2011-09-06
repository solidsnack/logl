{-# LANGUAGE OverloadedStrings
           , RecordWildCards
  #-}
{-| YAML serialization to a single-line format that escapes control
    characters and high-bytes. Suitable for inclusion in syslog messages.
 -}
module Language.LogL.FlatYAML (encodeFlat, escapeFlat) where

import Control.Applicative
import Data.ByteString
import qualified Data.ByteString.Char8
import qualified Data.List as List
import Data.Monoid
import Numeric
import Text.Printf

import Blaze.ByteString.Builder
import Data.Object.Yaml (YamlObject, YamlScalar(..))
import Data.Object


{-| Encode YAML in a very plain style, escaping newlines, control characters
    and high bytes and wrapping in quotes as necessary.
 -}
encodeFlat                  ::  YamlObject -> Builder
encodeFlat (Mapping pairs)   =  compound "{ " (encodeKV   <$> pairs) " }"
encodeFlat (Sequence items)  =  compound "[ " (encodeFlat <$> items) " ]"
encodeFlat (Scalar s)        =  fromByteString (escapeFlat s)

{-| Escape high bytes, control characters, newlines and YAML quoting
    characters, wrapping in quotes if escaping is needed or any YAML special
    characters are encountered.
 -}
escapeFlat                  ::  YamlScalar -> ByteString
escapeFlat YamlScalar{..} | quotingNeeded = toByteString (quote builder)
                          | otherwise     = toByteString builder
 where
  (quotingNeeded, builder)   =  foldl' f (False, mempty) value
  quote b                    =  q `mappend` b `mappend` q
  q                          =  (fromWord8 . fromIntegral . fromEnum) '"'
  f (quotingNeeded, soFar) byte
    | isControlOrHigh        =  (True,          soFar'' escaped)
    | isQuoteMagic           =  (True,          soFar'' escaped)
    | isYAMLSpecial          =  (True,          soFar' byte)
    | otherwise              =  (quotingNeeded, soFar' byte)
   where
    isYAMLSpecial            =  byte `Data.ByteString.elem` "!#:{}[],?&* "
    isQuoteMagic             =  byte `Data.ByteString.elem` "\"\\"
    isControlOrHigh          =  byte < 0x20 || byte > 0x7e
    soFar'                   =  mappend soFar . fromWord8
    soFar''                  =  mappend soFar . fromByteString
    escaped                  =  case (toEnum . fromIntegral) byte of
      '\0'                  ->  "\\0"
      '\a'                  ->  "\\a"
      '\b'                  ->  "\\b"
      '\t'                  ->  "\\t"
      '\n'                  ->  "\\n"
      '\v'                  ->  "\\v"
      '\f'                  ->  "\\f"
      '\r'                  ->  "\\r"
      '\ESC'                ->  "\\e"
      '\\'                  ->  "\\\\"
      '"'                   ->  "\\\""
      _ -> Data.ByteString.Char8.pack (printf "\\x%02x" byte)

encodeKV (k, v) = mappend (fromByteString (escapeFlat k `mappend` ": "))
                          (encodeFlat v)

compound delimiterLeft vals delimiterRight =
  delimiterLeft' `mappend` mconcat vals' `mappend` delimiterRight'
 where
  delimiterLeft'             =  fromByteString delimiterLeft
  vals'                      =  List.intersperse (fromByteString ", ") vals
  delimiterRight'            =  fromByteString delimiterRight

