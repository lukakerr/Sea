module Sea.Evaluator (evaluator) where

import Data.Maybe
import Debug.Trace
import Sea.Syntax
import qualified Sea.Environment as E

type VEnv = E.Env Value

type OpFun = (Int -> Int -> Int)

evaluator :: Program -> Value
evaluator (Main e) = evalE E.empty e

evalE :: VEnv -> Exp -> Value
evalE v End = Nil
evalE v (Show e) = evalS v e
evalE v (Return e) = evalS v e
evalE v (Assignment a) = evalA v a

evalA :: VEnv -> Assign -> Value
evalA v (Equals i s e) = let
    v' = E.add v (i, stmt)
    stmt = evalS v' s
  in evalE v' e

evalS :: VEnv -> Statement -> Value
evalS v (Var i) = fromMaybe (error $ "Variable " ++ i ++ " not in scope") $ E.get v i
evalS v (Const c) = c
evalS v (App (App (Prim p) e1) e2) = evalPrimOp (evalS v e1) p (evalS v e2)

evalPrimOp :: Value -> Op -> Value -> Value
evalPrimOp e1 op e2 = case (e1, e2) of
  (Number e1', Number e2') ->
    case op of
      Plus -> Number $ e1' + e2'
      Minus -> Number $ e1' - e2'
      Times -> Number $ e1' * e2'
      Divide -> Number $ oper e1' quot e2'
      Modulus -> Number $ oper e1' rem e2'
      EqS -> Boolean $ e1' == e2'
      NeQ -> Boolean $ e1' /= e2'
      Gt -> Boolean $ e1' > e2'
      Lt -> Boolean $ e1' < e2'
      GtE -> Boolean $ e1' >= e2'
      LtE -> Boolean $ e1' <= e2'
  (Boolean e1', Boolean e2') ->
    case op of
      EqS -> Boolean $ e1' == e2'
      NeQ -> Boolean $ e1' /= e2'
      And -> Boolean $ e1' && e2'
      Or -> Boolean $ e1' || e2'
  (e1', e2') ->
    error $ "Can't compare " ++ show e1' ++ " and " ++ show e2'

-- util function to ensure operations where division by 0 could occur throws an error
oper :: Int -> OpFun -> Int -> Int
oper _ _ 0 = error "Division by zero is invalid"
oper a f b = a `f` b
