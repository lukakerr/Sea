module Sea.Parser (parser) where

import Sea.Syntax
import Sea.Program

parser :: Lexemes -> Either Exception Exp
parser [] = Right End
parser ls = do
  (e, tokens) <- runProgram $ parseFunction ls
  case tokens of
    [] -> Right e
    _  -> Left $ ParseError $ "couldn't parse all tokens " ++ show tokens

parseFunction :: Lexemes -> Program (Exp, Lexemes)
parseFunction ((Kwd Fn, _) : (Identifier "main", _) : (LBrace, _) : (RBrace, _) : (LParen, _) : e) =
  parseExp $ init e
parseFunction _ = exception $ ParseError "only supports a single main {} declaration"

parseExp :: Lexemes -> Program (Exp, Lexemes)
parseExp [] = return  (End, [])
parseExp e@((a, _) : (Operator o, _) : ts) = parseOperation e
parseExp e@((Kwd kwd, _) : ts) = parseKeyword e
parseExp ((Num n, _) : ts) = return (Const (Number n), ts)
parseExp ((Str s, _) : ts) = return (Const (String s), ts)
parseExp ((Bln b, _) : ts) = return (Const (Boolean b), ts)
parseExp ((Identifier i, _) : ts) = return (Var i, ts)
parseExp ((LParen, l) : ts) = do
  (exp, ts) <- parseExp ts
  case ts of
    ((RParen, _) : ts') -> return (exp, ts')
    ((_, _) : _) -> exception $ ExpectedToken ")" l
parseExp ts = parseAssignment ts

parseKeyword :: Lexemes -> Program (Exp, Lexemes)
parseKeyword (k@(Kwd kwd, _) : ts) = let
    f = case kwd of
      Ret -> parseReturn
      If -> parseIf
  in f (k : ts)

parseOperation :: Lexemes -> Program (Exp, Lexemes)
parseOperation (a : (Operator o, _) : ts) = do
  a'       <- parseExp [a]
  (e, ts') <- parseExp ts
  return (App (App (Prim o) (fst a')) e, ts')

parseAssignment :: Lexemes -> Program (Exp, Lexemes)
parseAssignment ((DataType t, _) : (Identifier i, _) : (Operator Eq, _) : ts) = do
  (e , ts' ) <- parseExp ts
  (e', ts'') <- parseExp ts'
  return (Assignment i Eq e e', ts'')
parseAssignment ts = parseOperation ts

parseReturn :: Lexemes -> Program (Exp, Lexemes)
parseReturn ((Kwd Ret, _) : ts) = do
  (e, ts') <- parseExp ts
  return (Return e, ts')

parseIf :: Lexemes -> Program (Exp, Lexemes)
parseIf ((Kwd If, _) : (LBrace, ll) : ts) = do
  (cond, ts') <- parseExp ts
  case ts' of
    ((RBrace, _) : (LParen, l) : ts') -> do
      (trueBranch, ts'') <- parseExp ts'
      case ts'' of
        ((RParen, _) : (Kwd Else, _) : (LParen, l) : tss) -> do
          (falseBranch, tss') <- parseExp tss
          case tss' of
            ((RParen, _) : tss'') -> return (IfElse cond trueBranch falseBranch, tss'')
            _ -> exception $ ExpectedToken ")" l
        ((RParen, _) : (Kwd Else, l) : tss) -> exception $ ExpectedToken "(" l
        ((RParen, l) : tss) -> exception $ ExpectedToken "else keyword" l
        _ -> exception $ ExpectedToken ")" l
    ((RBrace, l) : _) -> exception $ ExpectedToken "(" l
    _ -> exception $ ExpectedToken "}" ll
parseIf ((Kwd If, l) : ts) = exception $ ExpectedToken "{" l
