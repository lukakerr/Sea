module Sea.Program
  ( Exception(..)
  , Program
  , runProgram
  , exception
  , showException
  )
where

import Sea.Syntax
import Control.Monad
import Control.Applicative

data Exception =
  UnexpectedToken String Line
  | ExpectedToken String Line
  | OutOfScope String
  | ParseError String

newtype Program a = Program (Either Exception a)

instance Monad Program where
  (Program pg) >>= f =
      case pg of
        Left ex -> Program (Left ex)
        Right v -> f v
  return v = Program (Right v)

instance Applicative Program where
  pure = return
  (<*>) = ap

instance Functor Program where
  fmap = liftM

runProgram :: Program a -> Either Exception a
runProgram (Program f) = f

exception :: Exception -> Program a
exception = Program . Left

showException :: Exception -> String
showException (UnexpectedToken s l) = "Unexpected token " ++ s ++ " at line " ++ show l
showException (ExpectedToken s l) = "Expected " ++ s ++ " at line " ++ show l
showException (OutOfScope s) = "Variable " ++ s ++ " is out of scope"
showException (ParseError s) = "Parsing error " ++ s
