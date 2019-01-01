module Sea.Evaluator (evaluator) where

import Data.Maybe
import Sea.Syntax
import Sea.Program
import qualified Sea.Environment as E

type VEnv = E.Env Value

type OpFun = (Int -> Int -> Int)

evaluator :: Exp -> Value
evaluator = evalE E.empty

-- evaluate an expression
evalE :: VEnv -> Exp -> Value
evalE v End = Nil
evalE v (Const c) = c
evalE v (Var i) =
  fromMaybe (error $ "Variable " ++ i ++ " not in scope") $ E.get v i
evalE v (Return e) = evalE v e
evalE v e@(IfElse e1 e2 e3 e4) = evalIf v e
evalE v e@(WhileLoop e1 e2 e3) = evalWhile v e
evalE v e@(App e1 o e2) = evalApp v e
evalE v e = evalA v e

-- evaluate an assignment
evalA :: VEnv -> Exp -> Value
evalA v (Assignment i Eq s e) = let
    v' = E.set v (i, stmt)
    stmt = evalE v' s
  in evalE v' e
evalA v (Assignment i o s e) = evalMutablePrimOp v i o s e

-- evaluate an application
evalApp :: VEnv -> Exp -> Value
evalApp v (App e1 o e2) = let
    e1' = evalE v e1
    e2' = evalE v e2
  in evalPrimOp e1' o e2'

-- evaluate an if statement
evalIf :: VEnv -> Exp -> Value
evalIf v (IfElse e1 e2 e3 e4) = case evalE v e1 of
  (Boolean e1') -> if e1' then evalE v e2 else evalE v e3
  _             -> error "Invalid boolean condition"

-- evaluate a while statement
evalWhile :: VEnv -> Exp -> Value
evalWhile v wl@(WhileLoop e1 e2 e3) = case evalE v e1 of
  (Boolean e1') -> if e1' then seq (evalE v e2) (evalE v wl) else evalE v e3
  _             -> error "Invalid boolean condition"

-- evaluate a primitive operation
evalPrimOp :: Value -> Op -> Value -> Value
evalPrimOp e1 op e2 = case (e1, e2) of
  (Number e1', Number e2') ->
    case op of
      Gt -> Boolean $ e1' > e2'
      Lt -> Boolean $ e1' < e2'
      GtE -> Boolean $ e1' >= e2'
      LtE -> Boolean $ e1' <= e2'
      EqS -> Boolean $ e1' == e2'
      NeQ -> Boolean $ e1' /= e2'
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
  (String e1', String e2') ->
    case op of
      EqS -> Boolean $ e1' == e2'
      NeQ -> Boolean $ e1' /= e2'
      o | o `elem` [Plus, PlusEq] -> String $ e1' ++ e2'
      _ -> error $ "Can't perform " ++ show op ++ " on two String values"
  (e1', e2') -> error $ "Can't compare " ++ show e1' ++ " and " ++ show e2'

-- evaluate a mutable prim op and set its value in the environment
evalMutablePrimOp :: VEnv -> Id -> Op -> Exp -> Exp -> Value
evalMutablePrimOp v i o e1 e2 = let
    e1' = evalE v e1
    i'  = evalE v $ Var i
    i'' = evalPrimOp i' o e1'
    v'  = E.set v (i, i'')
  in  evalE v' e2

-- ensure operations where division by 0 could occur throws an error
oper :: Int -> OpFun -> Int -> Int
oper _ _ 0 = error "Division by zero is invalid"
oper a f b = a `f` b
