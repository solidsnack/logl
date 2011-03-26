{-# LANGUAGE TemplateHaskell
  #-}
module Language.LogL.Macros where

import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import Data.List
import Control.Applicative


{-| Just a macro to pull in a file.
 -}
text f                       =  lift =<< runIO (readFile f)

{-| Read in a file and split out its text blocks, making a tuple. 
 -}
text_blocks f                =  do
  contents                  <-  runIO $ blocks <$> readFile f
  return $ TupE (LitE . stringL <$> contents)

blocks                       =  unfoldr step . lines
 where
  step s | null block        =  Nothing
         | otherwise         =  Just (unlines block, s')
   where
    (block, s')              =  (span (/= "") . dropWhile (== "")) s

{-| Extract the version from the Cabal file and place it here as string.
 -}
version                      =  (lift =<<) . runIO $ do
  s                         <-  readFile "logl.cabal"
  case filter version (lines s) of
    v:_                     ->  return (break_version v)
    [ ]                     ->  error "Could not find version :("
 where
  version line               =  "version" == take 7 line
  break_version = snd . break (/= ' ') . drop 1 . snd . break (== ':')

