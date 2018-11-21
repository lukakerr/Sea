module Sea.Parser (parser) where

import Sea.Syntax

parser :: [Token] -> Program
parser [] = Main (Return (Const (Number 0)))
parser x = case parseTokens x of
  (e, []) -> Main e
  (e, tokens) -> error $ "Couldn't parse all tokens " ++ show tokens

parseTokens :: [Token] -> (Exp, [Token])
parseTokens [] = (End, [])
parseTokens ts = parseFunction ts

parseFunction :: [Token] -> (Exp, [Token])
parseFunction (Kwd Fn : Identifier "main" : LBrace : RBrace : LParen : e) =
  parseExp $ init e
parseFunction _ = error "Only supports a single main {} declaration"

parseExp :: [Token] -> (Exp, [Token])
parseExp (k@(Kwd kwd) : ts) = let
    f = case kwd of
      Print -> parsePrint
      Ret -> parseReturn
    (e, ts') = f (k : ts)
  in if null ts' then (e, ts') else parseExp ts'
parseExp ts = let
    (e, ts') = parseAssignment ts
  in if null ts' then (e, ts') else parseExp ts'

parseAssignment :: [Token] -> (Exp, [Token])
parseAssignment (DataType t : Identifier i : Operator Eq : ts) =
  let
    (e', ts') = parseOperation ts
    (e'', ts'') = parseExp ts'
  in (Assignment (AsignOp Eq i e' e''), ts'')
parseAssignment (Identifier i : Operator o : ts) =
  let
    (s, ts') = parseOperation ts
    (s', ts'') = parseExp ts'
  in (Assignment (AsignOp o i s s'), ts'')

parsePrint :: [Token] -> (Exp, [Token])
parsePrint (Kwd Print : ts) = let
    (stmt, ts') = parseOperation ts
  in (Show stmt, ts')

parseReturn :: [Token] -> (Exp, [Token])
parseReturn (Kwd Ret : ts) = let
    (stmt, ts') = parseOperation ts
  in (Return stmt, ts')

parseOperation :: [Token] -> (Statement, [Token])
parseOperation (a : Operator o : b : ts) =
  let a' = fst $ parseAtom [a] in
    case b of
      LParen ->
        let (b', bs) = parseAtom (b:ts) in
          (App (App (Prim o) a') b', bs)
      _ ->
        let b' = fst $ parseAtom [b] in
          (App (App (Prim o) a') b', ts)
parseOperation x = parseAtom x

parseAtom :: [Token] -> (Statement, [Token])
parseAtom (Bln b : ts) = (Const (Boolean b), ts)
parseAtom (Num n : ts) = (Const (Number n), ts)
parseAtom (Str s : ts) = (Const (String s), ts)
parseAtom (Identifier i : ts) = (Var i, ts)
parseAtom (LParen : ts) = case parseOperation ts of
  (exp, RParen : ts') -> (exp, ts')
  (exp, ts') -> error "Expected )"
parseAtom (t:ts) = error $ "Unexpected token " ++ show t
