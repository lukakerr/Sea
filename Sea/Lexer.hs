module Sea.Lexer (lexer) where

import Sea.Syntax
import Data.Char(isSpace, isDigit, isAlpha)

lexer :: (String, Line) -> Lexemes
lexer ([], l) = []

-- remove whitespace, isSpace handles all others
lexer (' ':cs, l) = lexer (cs, l)
lexer ('\n':cs, l) = lexer (cs, l + 1)
lexer (c:cs, l) | isSpace c = lexer (cs, l)

-- arrow
lexer ('-' : '>' : cs, l) = (Arrow, l) : lexer (cs, l)

-- arithmetic operations
lexer ('+':'=':cs, l) = (Operator PlusEq, l) : lexer (cs, l)
lexer ('-':'=':cs, l) = (Operator MinusEq, l) : lexer (cs, l)
lexer ('*':'=':cs, l) = (Operator TimesEq, l) : lexer (cs, l)
lexer ('/':'=':cs, l) = (Operator DivideEq, l) : lexer (cs, l)
lexer ('%':'=':cs, l) = (Operator ModulusEq, l) : lexer (cs, l)
lexer ('+':cs, l) = (Operator Plus, l) : lexer (cs, l)
lexer ('-':cs, l) = (Operator Minus, l) : lexer (cs, l)
lexer ('*':cs, l) = (Operator Times, l) : lexer (cs, l)
lexer ('/':cs, l) = (Operator Divide, l) : lexer (cs, l)
lexer ('%':cs, l) = (Operator Modulus, l) : lexer (cs, l)

-- arithmetic comparisons
lexer ('>':'=':cs, l) = (Operator GtE, l) : lexer (cs, l)
lexer ('<':'=':cs, l) = (Operator LtE, l) : lexer (cs, l)
lexer ('=':'=':cs, l) = (Operator EqS, l) : lexer (cs, l)
lexer ('!':'=':cs, l) = (Operator NeQ, l) : lexer (cs, l)
lexer ('&':'&':cs, l) = (Operator And, l) : lexer (cs, l)
lexer ('|':'|':cs, l) = (Operator Or, l) : lexer (cs, l)
lexer ('=':cs, l) = (Operator Eq, l) : lexer (cs, l)
lexer ('>':cs, l) = (Operator Gt, l) : lexer (cs, l)
lexer ('<':cs, l) = (Operator Lt, l) : lexer (cs, l)

-- braces, parentheses
lexer ('(':cs, l) = (LParen, l) : lexer (cs, l)
lexer (')':cs, l) = (RParen, l) : lexer (cs, l)
lexer ('{':cs, l) = (LBrace, l) : lexer (cs, l)
lexer ('}':cs, l) = (RBrace, l) : lexer (cs, l)

-- digits
lexer (c:cs, l) | isDigit c = let
  (numString, rest) = span isDigit (c:cs)
    in (Num (read numString), l) : lexer (rest, l)

-- booleans
lexer ('t':'r':'u':'e':cs, l) = (Bln True, l) : lexer (cs, l)
lexer ('f':'a':'l':'s':'e':cs, l) = (Bln False, l) : lexer (cs, l)

-- single quoted strings
lexer ('\'':cs, l) = let
  (tok, rest) = captureStr cs '\''
    in (tok, l) : lexer (rest, l)

-- double quoted strings
lexer ('"':cs, l) = let
  (tok, rest) = captureStr cs '"'
    in (tok, l) : lexer (rest, l)

-- words
lexer (c:cs, l) | isAlpha c = let
  (word, rest) = span isWord (c : cs)
    in (captureKwd word, l) : lexer (rest, l)

-- remove comments
lexer (';':';':cs, l) = lexer (dropWhile (/= '\n') cs, l)

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
