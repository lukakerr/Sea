module Sea.Parser (parser) where

import Sea.Syntax

parser :: [Token] -> Exp
parser [] = End
parser x = case parseTokens x of
  (e, []) -> e
  (e, tokens) -> error $ "Couldn't parse all tokens " ++ show tokens

parseTokens :: [Token] -> (Exp, [Token])
parseTokens [t] = parseAtom [t]
parseTokens ts = parseOperation ts

parseOperation :: [Token] -> (Exp, [Token])
parseOperation (a : Operator o : b : ts) =
  let a' = fst $ parseAtom [a] in
    case b of
      LParen ->
        let
          (b', bs) = parseAtom (b:ts)
        in (App (App (Prim o) a') b', bs)
      _ ->
        let
          b' = fst $ parseAtom [b]
        in (App (App (Prim o) a') b', ts)
parseOperation x = parseAtom x

parseAtom :: [Token] -> (Exp, [Token])
parseAtom (Bln b : ts) = (Const (Boolean b), ts)
parseAtom (Num n : ts) = (Const (Number n), ts)
parseAtom (Str s : ts) = (Const (String s), ts)
parseAtom (LParen : ts) = case parseTokens ts of
  (exp, RParen : ts') -> (exp, ts')
  (exp, ts') -> error "Expected )"
parseAtom (t:ts) = error "Unexpected token"
