module Sea.Syntax where

type Id = String

-- tokens the lexer can produce
data Token =
  Arrow
  | Operator Op
  | Kwd Keyword
  | DataType Type
  | Identifier Id
  | Num Int | Str String | Bln Bool
  | LBrace | RBrace | LParen | RParen
  deriving (Show)

data Keyword = If | Run | Else | While | Ret | Fn
  deriving (Show)

-- data types
data Type = N | S | B
  deriving (Show)

data Op =
  Gt | Lt | GtE | LtE
  | Plus | Minus | Times | Divide | Modulus
  | Eq | PlusEq | MinusEq | TimesEq | DivideEq | ModulusEq
  deriving (Show)

-- expressions the parser can produce
data Exp =
  End
  | Const Value
  | Prim Op
  | Var Id
  | App Exp Exp
  deriving (Show)

-- values the program can evaluate to
data Value =
  Number Int
  | Boolean Bool
  | String String
  | Nil
  deriving (Show)

showValue :: Value -> String
showValue (Number n) = show n
showValue (Boolean b) = show b
showValue Nil = "Nil"
