module Sea.Evaluator (evaluator) where

import Data.Maybe
import Sea.Syntax
import qualified Sea.Environment as E

type VEnv = E.Env Value

type OpFun = (Int -> Int -> Int)

evaluator :: Program -> Value
evaluator (Main e) = evalE E.empty e

-- evaluate an expression
evalE :: VEnv -> Exp -> Value
evalE v End = Nil
evalE v (Show e) = evalS v e
evalE v (Return e) = evalS v e
evalE v (Assignment a) = evalA v a

-- evaluate an assignment
evalA :: VEnv -> Assign -> Value
evalA v (AsignOp Eq i s e) = let
    v' = E.add v (i, stmt)
    stmt = evalS v' s
  in evalE v' e
evalA v (AsignOp o i s e) = let
    s' = evalS v s
    i' = evalS v $ Var i
    i'' = evalPrimOp i' o s'
    v' = E.add v (i, i'')
  in evalE v' e

-- evaluate a statement
evalS :: VEnv -> Statement -> Value
evalS v (Var i) = fromMaybe (error $ "Variable " ++ i ++ " not in scope") $ E.get v i
evalS v (Const c) = c
evalS v (App (App (Prim p) e1) e2) = evalPrimOp (evalS v e1) p (evalS v e2)

-- evaluate a primitive operation
evalPrimOp :: Value -> Op -> Value -> Value
evalPrimOp e1 op e2 = case (e1, e2) of
  (Number e1', Number e2') ->
    case op of
      EqS -> Boolean $ e1' == e2'
      NeQ -> Boolean $ e1' /= e2'
      Gt -> Boolean $ e1' > e2'
      Lt -> Boolean $ e1' < e2'
      GtE -> Boolean $ e1' >= e2'
      LtE -> Boolean $ e1' <= e2'
      o | o `elem` [Plus, PlusEq] -> Number $ e1' + e2'
      o | o `elem` [Minus, MinusEq] -> Number $ e1' - e2'
      o | o `elem` [Times, TimesEq] -> Number $ e1' * e2'
      o | o `elem` [Divide, DivideEq] -> Number $ oper e1' quot e2'
      o | o `elem` [Modulus, ModulusEq] -> Number $ oper e1' rem e2'
      _ -> error $ "Can't perform " ++ show op ++ " on two Number values"
  (Boolean e1', Boolean e2') ->
    case op of
      EqS -> Boolean $ e1' == e2'
      NeQ -> Boolean $ e1' /= e2'
      And -> Boolean $ e1' && e2'
      Or -> Boolean $ e1' || e2'
      _ -> error $ "Can't perform " ++ show op ++ " on two Boolean values"
  (e1', e2') ->
    error $ "Can't compare " ++ show e1' ++ " and " ++ show e2'

-- util function to ensure operations where division by 0 could occur throws an error
oper :: Int -> OpFun -> Int -> Int
oper _ _ 0 = error "Division by zero is invalid"
oper a f b = a `f` b
