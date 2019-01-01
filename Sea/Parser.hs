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
parseExp [] = return (End, [])
parseExp ((LParen, l) : ts) = do
  (exp, ts') <- parseExp ts
  case ts' of
    ((RParen, _) : ts'') -> return (exp, ts'')
    _                    -> exception $ ExpectedToken ")" l
parseExp e@((Kwd kwd, _) : ts) = parseKeyword e
parseExp e@((DataType t, _) : (Identifier i, _) : (Operator o, _) : ts) = parseAssignment e
parseExp e@((Identifier i, _) : (Operator o, _) : ts)
  | o `elem` [Eq, PlusEq, MinusEq, TimesEq, DivideEq, ModulusEq] = parseAssignment e
parseExp e@((a, _) : (Operator o, _) : ts) = parseOperation e
parseExp ((Identifier i, _) : ts) = return (Var i, ts)
parseExp ((Num n, _) : ts) = return (Const (Number n), ts)
parseExp ((Str s, _) : ts) = return (Const (String s), ts)
parseExp ((Bln b, _) : ts) = return (Const (Boolean b), ts)

parseKeyword :: Lexemes -> Program (Exp, Lexemes)
parseKeyword (k@(Kwd kwd, _) : ts) = let
    f = case kwd of
      Ret -> parseReturn
      If -> parseIf
      While -> parseWhile
  in f (k : ts)

parseOperation :: Lexemes -> Program (Exp, Lexemes)
parseOperation ls@(a : (Operator o, _) : ts) = do
  a'       <- parseExp [a]
  (e, ts') <- parseExp ts
  return (App (fst a') o e, ts')

parseAssignment :: Lexemes -> Program (Exp, Lexemes)
parseAssignment ((DataType t, _) : (Identifier i, _) : (Operator Eq, _) : ts) = do
  (e , ts' ) <- parseExp ts
  (e', ts'') <- parseExp ts'
  return (Assignment i Eq e e', ts'')
parseAssignment ((Identifier i, _) : (Operator o, _) : ts) = do
  (e , ts' ) <- parseExp ts
  (e', ts'') <- parseExp ts'
  return (Assignment i o e e', ts'')

parseReturn :: Lexemes -> Program (Exp, Lexemes)
parseReturn ((Kwd Ret, _) : ts) = do
  (e, ts') <- parseExp ts
  return (Return e, ts')

parseIf :: Lexemes -> Program (Exp, Lexemes)
parseIf ((Kwd If, _) : (LBrace, l) : ts) = do
  (cond, ts') <- parseExp ts
  case ts' of
    ((RBrace, _) : (LParen, l) : ts'') -> do
      (trueBranch, tss) <- parseExp ts''
      case tss of
        ((RParen, _) : (Kwd Else, _) : (LParen, l) : tss') -> do
          (falseBranch, tss'') <- parseExp tss'
          case tss'' of
            ((RParen, _) : tsss) -> do
              (next, tsss') <- parseExp tsss
              return (IfElse cond trueBranch falseBranch next, tsss')
            _ -> exception $ ExpectedToken ")" l
        ((RParen, _) : (Kwd Else, l) : _) -> exception $ ExpectedToken "(" l
        ((RParen, l) : _) -> exception $ ExpectedToken "else keyword" l
        _ -> exception $ ExpectedToken ")" l
    ((RBrace, l) : _) -> exception $ ExpectedToken "(" l
    _                 -> exception $ ExpectedToken "}" l
parseIf ((Kwd If, l) : _) = exception $ ExpectedToken "{" l

parseWhile :: Lexemes -> Program (Exp, Lexemes)
parseWhile ((Kwd While, _) : (LBrace, l) : ts) = do
  (cond, ts') <- parseExp ts
  case ts' of
    ((RBrace, _) : (LParen, l) : ts'') -> do
      (exp, tss) <- parseExp ts''
      case tss of
        ((RParen, _) : tss') -> do
          (next, tss'') <- parseExp tss'
          return (WhileLoop cond exp next, tss'')
        _ -> exception $ ExpectedToken ")" l
    ((RBrace, l) : _) -> exception $ ExpectedToken "(" l
    _                 -> exception $ ExpectedToken "}" l
parseWhile ((Kwd While, l) : _) = exception $ ExpectedToken "{" l
