module Sea.Parser (parser) where

import Sea.Syntax

parser :: [Token] -> Program
parser [] = Main (Return (Const (Number 0)))
parser x = case parseFunction x of
  (e, []) -> Main e
  (e, tokens) -> error $ "Couldn't parse all tokens " ++ show tokens

parseFunction :: [Token] -> (Exp, [Token])
parseFunction (Kwd Fn : Identifier "main" : LBrace : RBrace : LParen : e) =
  parseExp $ init e
parseFunction _ = error "Only supports a single main {} declaration"

parseExp :: [Token] -> (Exp, [Token])
parseExp [] = (End, [])
parseExp e@(a : Operator o : ts) = parseOperation e
parseExp e@(Kwd kwd : ts) = parseKeyword e
parseExp (Num n : ts) = (Const (Number n), ts)
parseExp (Str s : ts) = (Const (String s), ts)
parseExp (Bln b : ts) = (Const (Boolean b), ts)
parseExp (Identifier i : ts) = (Var i, ts)
parseExp (LParen : ts) = case parseExp ts of
  (exp, RParen : ts') -> (exp, ts')
  (exp, ts') -> error "Expected )"
parseExp ts = parseAssignment ts

parseKeyword :: [Token] -> (Exp, [Token])
parseKeyword (k@(Kwd kwd) : ts) = let
    f = case kwd of
      Ret -> parseReturn
      If -> parseIf
  in f (k : ts)

parseOperation :: [Token] -> (Exp, [Token])
parseOperation (a : Operator o : ts) = let
    a' = fst $ parseExp [a]
    (e, ts') = parseExp ts
  in (App (App (Prim o) a') e, ts')

parseAssignment :: [Token] -> (Exp, [Token])
parseAssignment (DataType t : Identifier i : Operator Eq : ts) =
  let
    (e, ts') = parseExp ts
    (e', ts'') = parseExp ts'
  in (Assignment i Eq e e', ts'')
parseAssignment ts = parseOperation ts

parseReturn :: [Token] -> (Exp, [Token])
parseReturn (Kwd Ret : ts) = let
    (stmt, ts') = parseExp ts
  in (Return stmt, ts')

parseIf :: [Token] -> (Exp, [Token])
parseIf (Kwd If : LBrace : ts) = case parseExp ts of
  (cond, RBrace : Kwd Run : ts') -> case ts' of
    (LParen : ts'') -> case parseExp ts'' of
      (trueBranch, RParen : Kwd Else : LParen : tss) -> case parseExp tss of
        (falseBranch, RParen : tss') -> (IfElse cond trueBranch falseBranch, tss')
        _                            -> error "Expected )"
      (_, RParen : Kwd Else : _) -> error "Expected ("
      (_, RParen : _)            -> error "Expected else keyword"
      _                          -> error "Expected )"
    _ -> error "Expected ("
  (_, RBrace : _) -> error "Expected run keyword"
  _               -> error "Expected }"
parseIf (Kwd If : ts) = error "Expected {"
