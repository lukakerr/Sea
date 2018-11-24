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
parseExp e@(a : Operator o : b : ts) = parseAssignment e
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
      Print -> parsePrint
      Ret -> parseReturn
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

parsePrint :: [Token] -> (Exp, [Token])
parsePrint (Kwd Print : ts) = let
    (stmt, ts') = parseExp ts
    (e, ts'') = parseExp ts'
  in (Show stmt e, ts'')

parseReturn :: [Token] -> (Exp, [Token])
parseReturn (Kwd Ret : ts) = let
    (stmt, ts') = parseExp ts
  in (Return stmt, ts')
