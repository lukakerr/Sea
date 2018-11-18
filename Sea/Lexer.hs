module Sea.Lexer (lexer) where

import Sea.Syntax
import Data.Char(isSpace, isDigit, isAlpha)

lexer :: String -> [Token]
lexer [] = []

-- remove whitespace, isSpace handles all others
lexer (' ':cs) = lexer cs
lexer (c:cs) | isSpace c = lexer cs

-- arrow
lexer ('-' : '>' : cs) = Arrow : lexer cs

-- arithmetic operations
lexer ('+':'=':cs) = Operator PlusEq : lexer cs
lexer ('-':'=':cs) = Operator MinusEq : lexer cs
lexer ('*':'=':cs) = Operator TimesEq : lexer cs
lexer ('/':'=':cs) = Operator DivideEq : lexer cs
lexer ('%':'=':cs) = Operator ModulusEq : lexer cs
lexer ('+':cs) = Operator Plus : lexer cs
lexer ('-':cs) = Operator Minus : lexer cs
lexer ('*':cs) = Operator Times : lexer cs
lexer ('/':cs) = Operator Divide : lexer cs
lexer ('%':cs) = Operator Modulus : lexer cs

-- arithmetic comparisons
lexer ('>':'=':cs) = Operator GtE : lexer cs
lexer ('<':'=':cs) = Operator LtE : lexer cs
lexer ('=':cs) = Operator Eq : lexer cs
lexer ('>':cs) = Operator Gt : lexer cs
lexer ('<':cs) = Operator Lt : lexer cs

-- braces, parentheses
lexer ('(':cs) = LParen : lexer cs
lexer (')':cs) = RParen : lexer cs
lexer ('{':cs) = LBrace : lexer cs
lexer ('}':cs) = RBrace : lexer cs

-- digits
lexer (c:cs) | isDigit c = let
  (numString, rest) = span isDigit (c:cs)
    in Num (read numString) : lexer rest

-- booleans
lexer ('t':'r':'u':'e':cs) = Bln True : lexer cs
lexer ('f':'a':'l':'s':'e':cs) = Bln False : lexer cs

-- single quoted strings
lexer ('\'':cs) = let
  (tok, rest) = captureStr cs '\''
    in tok : lexer rest

-- double quoted strings
lexer ('"':cs) = let
  (tok, rest) = captureStr cs '"'
    in tok : lexer rest

-- words
lexer (c:cs) | isAlpha c = let
  (word, rest) = span isWord (c : cs)
    in captureKwd word : lexer rest

-- remove comments
lexer (';':';':cs) = lexer $ dropWhile (/= '\n') cs

-- anything not handled
lexer _ = []

captureStr :: String -> Char -> (Token, String)
captureStr string quote = let
  (str, rest) = span (/= quote) string
  withoutTrailing = drop 1 rest
    in (Str str, withoutTrailing)

captureKwd :: String -> Token
captureKwd kwd = case kwd of
  "if" -> Kwd If
  "run" -> Kwd Run
  "else" -> Kwd Else
  "while" -> Kwd While
  "ret" -> Kwd Ret
  "fn" -> Kwd Fn
  "num" -> DataType N
  "bln" -> DataType B
  "str" -> DataType S
  w -> Identifier w

isWord :: Char -> Bool
isWord w = isAlpha w || isDigit w