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
  deriving (Show, Eq)

data Keyword = If | Run | Else | While | Ret | Fn | Print
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

-- programs the parser can produce
type Program = Function

-- fn main {} ( ... )
newtype Function = Main Exp
  deriving (Show, Eq)

data Exp =
  End
  | Assignment Assign
  | Return Statement
  | Show Statement
  deriving (Show, Eq)

data Assign = Equals Id Statement Exp
  deriving (Show, Eq)

data Statement = Const Value | Var Id | Prim Op | App Statement Statement
  deriving (Show, Eq)

-- values the program can evaluate to
data Value =
  Number Int
  | Boolean Bool
  | String String
  | Nil
  deriving (Show, Eq)

showValue :: Value -> String
showValue (Number n) = show n
showValue (Boolean b) = show b
showValue (String s) = show s
showValue _ = "Nil"
