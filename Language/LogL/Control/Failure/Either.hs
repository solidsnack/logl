
module Language.LogL.Control.Failure.Either where

import Control.Applicative
import Control.Monad.Error

import Control.Failure


instance Failure e (Either e) where
  failure e                  =  Left e

eitherExc onExc              =  either (const onExc) return

