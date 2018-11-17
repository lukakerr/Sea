module Sea.Syntax where

data Token =
  Kwd Keyword
  | Identifier String
  | Plus | Minus | Times | Divide | Modulus
  | Eq | PlusEq | MinusEq | TimesEq | DivideEq | ModulusEq
  | Gt | Lt | GtE | LtE
  | LBrace | RBrace | LParen | RParen
  | Num Int | Str String | Bln Bool
  deriving (Show)

data Keyword = If | Run | Else | While | Ret
  deriving (Show)