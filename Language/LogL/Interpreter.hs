module Language.LogL.Interpreter where

import Language.LogL.Syntax


data Interpreter = forall t. Interpreter { interpret :: LogL t -> IO (Result t) }


{-| Execution against SimpleDB with particular account credentials. 
 -}
sdb                         ::  t -> Interpreter
sdb keys_and_stuff           =  undefined
 where
  interpret logl             =  case logl of
    Alloc                   ->  undefined
    Free log_ID             ->  undefined
    Append log_ID message   ->  undefined
    ClipBefore entry_ID     ->  undefined

{-| Parallel execution against multiple backends, returning successfully if
    (and as soon as) more than half the backends return successfully. 
 -}
more_than_half              ::  [Interpreter] -> Interpreter
more_than_half backends      =  Interpreter {}
 where
  interpret logl             =  undefined -- Parallel execution goes here.
  n                          =  ceiling (fromIntegral (length backends) / 2)

{-| Parallel execution against multiple backends, returning successfully if
    and only if all return successfully.
 -}
all                         ::  [Interpreter] -> Interpreter
all                          =  undefined

