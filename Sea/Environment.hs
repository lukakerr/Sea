module Sea.Environment
  ( Env(..)
  , empty
  , get
  , set
  , setAll
  )
where

import qualified Data.Map as M

newtype Env e = Env (M.Map String e) deriving (Show, Eq)

empty :: Env e
empty = Env M.empty

get :: Env e -> String -> Maybe e
get (Env env) var = M.lookup var env

set :: Env e -> (String, e) -> Env e
set (Env env) (key, elt) = Env (M.insert key elt env)

setAll :: Env e -> [(String, e)] -> Env e
setAll (Env env) pairs = Env $ foldr (\(k, e) g -> M.insert k e g) env pairs
