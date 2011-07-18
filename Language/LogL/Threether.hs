{-# LANGUAGE StandaloneDeriving
  #-}

module Language.LogL.Threether where

import Control.Applicative

import Control.Failure


{-| Threether is sort of like Either but with a well-defined identity for
    for use with the @Alternative@ class. The alternative would be to
    implement @Control.Monad.Error.Error@ for many error types that don't
    logically have a null or empty error.
 -}
data Threether e t           =  Fail e | None | Result t
deriving instance (Eq e, Eq t) => Eq (Threether e t)
deriving instance (Ord e, Ord t) => Ord (Threether e t)
deriving instance (Show e, Show t) => Show (Threether e t)


threetherOK                 ::  (Monad m) => m t -> Threether e t -> m t
threetherOK _ (Result t)     =  return t
threetherOK m _              =  m

threether :: (e -> x) -> x -> (t -> x) -> Threether e t -> x
threether f _ _ (Fail e)     =  f e
threether _ x _ None         =  x
threether _ _ f (Result t)   =  f t


instance Failure e (Threether e) where
  failure e                  =  Fail e

instance Functor (Threether e) where
  fmap _ (Fail e)            =  Fail e
  fmap _ None                =  None
  fmap f (Result r)          =  Result (f r)

instance Monad (Threether e) where
  return                     =  Result
  Fail l   >>= _             =  Fail l
  None     >>= _             =  None
  Result r >>= k             =  k r

instance Applicative (Threether e) where
  pure                       =  Result
  Fail e   <*> _             =  Fail e
  None     <*> _             =  None
  Result f <*> r             =  fmap f r

instance Alternative (Threether e) where
  empty                      =  None
  Result t <|> _             =  Result t
  _        <|> n             =  n

