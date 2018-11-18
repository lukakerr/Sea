module Sea.Evaluator (evaluator) where

import Sea.Syntax

type OpFun = (Int -> Int -> Int)

evaluator :: Exp -> Value
evaluator End = Nil
evaluator (Const c) = c
evaluator (App (App (Prim p) e1) e2) = evalPrimOp (evaluator e1) p (evaluator e2)

evalPrimOp :: Value -> Op -> Value -> Value
evalPrimOp e1 op e2 = case (e1, e2) of
  (Number e1', Number e2') ->
    case op of
      Plus -> Number $ e1' + e2'
      Minus -> Number $ e1' - e2'
      Times -> Number $ e1' * e2'
      Divide -> Number $ oper e1' quot e2'
      Modulus -> Number $ oper e1' rem e2'

-- util function to ensure operations where division by 0 could occur throws an error
oper :: Int -> OpFun -> Int -> Int
oper _ _ 0 = error "Division by zero is invalid"
oper a f b = a `f` b
