{-# LANGUAGE OverloadedStrings
           , TypeSynonymInstances
  #-}
module Language.LogL.Pickle where

import Control.Applicative
import Control.Monad
import Data.ByteString.Char8 (ByteString, pack, unpack)
import qualified Data.ByteString.UTF8 as UTF8
import Data.String
import Data.Time.Clock
import Data.Time.Format
import Data.Word
import System.Locale (defaultTimeLocale)

import Data.Attoparsec.Char8


class Pickle t where
  i                         ::  ByteString -> Maybe t
  o                         ::  t -> ByteString

instance Pickle ByteString where
  i                          =  Just
  o                          =  id

instance Pickle String where
  i                          =  Just . UTF8.toString
  o                          =  UTF8.fromString

instance Pickle UTCTime where
  i b                        =  case parseOnly short_circuit_date_parser b of
    Left _                  ->  Nothing
    Right time              ->  Just time
  o                          =  pack . show
instance IsString UTCTime where
  fromString s =
    (either error id . parseOnly short_circuit_date_parser . pack) s

data Field = Year | Month | Day | Hour | Minute | Second | Subs

short_circuit_date_parser   ::  Parser UTCTime
short_circuit_date_parser    =  worker (Just Year) ""
 where
  worker                    ::  Maybe Field -> String -> Parser UTCTime
  worker Nothing s           =  case (timeParser . fill) s of Just t -> pure t
                                                              Nothing -> empty
   where
    fill s                   =  s ++ drop (length s) "0001-01-01T00:00:00Z"
    timeParser              ::  String -> Maybe UTCTime
    timeParser = parseTime defaultTimeLocale "%Y-%m-%dT%H:%M:%S%QZ"
  worker (Just field) soFar  =  case field of
    Year                    ->  worker (Just Month)
                                  =<< sequence [digit,digit,digit,digit]
    Month                   ->  done <|> r "-01" "-12" Day (then2d dash)
    Day                     ->  done <|> r "-01" "-31" Hour (then2d dash)
    Hour                    ->  done <|> r "T00" "T23" Minute (then2d t)
    Minute                  ->  done <|> r ":00" ":59" Second (then2d colon)
    Second                  ->  done <|> r ":00" ":59" Subs (then2d colon)
    Subs                    ->  done <|> do _ <- char '.'
                                            d <- unpack <$> upto 12 isDigit
                                            _ <- utc
                                            let subs = '.':d ++ "Z"
                                            worker Nothing (soFar ++ subs)
   where
    done                     =  utc >> endOfInput >> worker Nothing soFar
    utc                      =  choice . (string <$>)
                             $  ["Z", "+00", " UTC", "+00:00", "+0000"]
    t                        =  (char ' ' <|> char 'T') >> pure 'T'
    colon                    =  char ':'
    dash                     =  char '-'
    r lower upper next parser  =  do
      res                   <-  parser
      when (res < lower || res > upper) empty
      worker (Just next) (soFar ++ res)
    then2d                   =  sequence . (:[digit, digit])


upto                        ::  Word32 -> (Char -> Bool) -> Parser ByteString
upto count pred              =  scan count f
 where
  f n c | pred c && n > 0    =  Just (n - 1)
        | otherwise          =  Nothing

