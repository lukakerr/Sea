module Sea.Syntax where

data Token =
  Kwd Keyword
  | Identifier String
  | Plus | Minus | Times | Divide
  | Eq | PlusEq | MinusEq | TimesEq | DivideEq
  | Gt | Lt | GtE | LtE
  | LBrace | RBrace | LParen | RParen
  | Num Int
  deriving (Show)

data Keyword = If | Run | Else | While | Ret
  deriving (Show)