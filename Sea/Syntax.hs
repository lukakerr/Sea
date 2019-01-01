module Sea.Syntax where

type Line = Int
type Id = String
type Lexemes = [(Token, Line)]

-- tokens the lexer can produce
data Token =
  Arrow
  | Operator Op
  | Kwd Keyword
  | DataType Type
  | Identifier Id
  | Num Int | Str String | Bln Bool
  | LBrace | RBrace | LParen | RParen
  deriving (Show, Eq)

data Keyword = If | Else | While | Ret | Fn
  deriving (Show, Eq)

-- data types
data Type = N | S | B
  deriving (Show, Eq)

data Op =
  Gt | Lt | GtE | LtE
  | And | Or
  | Eq | EqS | NeQ
  | Plus | Minus | Times | Divide | Modulus
  | PlusEq | MinusEq | TimesEq | DivideEq | ModulusEq
  deriving (Show, Eq)

-- an expression to be evaluated next
type NextExp = Exp

-- expressions the parser could produce
data Exp =
  End
  | Var Id
  | Prim Op
  | Return Exp
  | Const Value
  | App Exp Op Exp
  | IfElse Exp Exp Exp NextExp
  | WhileLoop Exp Exp NextExp
  | Assignment Id Op Exp NextExp
  deriving (Show, Eq)

-- values the program can evaluate to
data Value =
  Number Int
  | Boolean Bool
  | String String
  | Nil
  deriving (Show, Eq)

-- returns a string representation of a Value
showValue :: Value -> String
showValue (Number n) = show n
showValue (Boolean b) = show b
showValue (String s) = s
showValue _ = "Nil"

-- returns a list of tokens from Lexemes
extractTokens :: Lexemes -> [Token]
extractTokens = map fst
