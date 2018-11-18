module Sea.Syntax where

type Id = String

-- tokens the lexer can produce
data Token =
  Kwd Keyword
  | Operator Op
  | Identifier Id
  | LBrace | RBrace | LParen | RParen
  | Num Int | Str String | Bln Bool
  deriving (Show)

data Keyword = If | Run | Else | While | Ret
  deriving (Show)

data Op =
  Plus | Minus | Times | Divide | Modulus
  | Eq | PlusEq | MinusEq | TimesEq | DivideEq | ModulusEq
  | Gt | Lt | GtE | LtE
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
