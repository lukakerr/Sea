module Sea.Environment
  ( Env(..)
  , empty
  , get
  , add
  , addAll
  )
where

import qualified Data.Map as M

newtype Env e = Env (M.Map String e) deriving (Show, Eq)

empty :: Env e
empty = Env M.empty

get :: Env e -> String -> Maybe e
get (Env env) var = M.lookup var env

add :: Env e -> (String, e) -> Env e
add (Env env) (key, elt) = Env (M.insert key elt env)

addAll :: Env e -> [(String, e)] -> Env e
addAll (Env env) pairs = Env $ foldr (\(k, e) g -> M.insert k e g) env pairs
