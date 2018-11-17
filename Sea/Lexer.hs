module Sea.Lexer (lexer) where

import Sea.Syntax
import Data.Char(isSpace, isDigit, isAlpha)
import Data.List(partition)

lexer :: String -> [Token]
lexer [] = []

-- remove whitespace, isSpace handles all others
lexer (' ':cs) = lexer cs
lexer (c:cs) | isSpace c = lexer cs

-- arithmetic operations
lexer ('+':'=':cs) = PlusEq : lexer cs
lexer ('-':'=':cs) = MinusEq : lexer cs
lexer ('*':'=':cs) = TimesEq : lexer cs
lexer ('/':'=':cs) = DivideEq : lexer cs
lexer ('+':cs) = Plus : lexer cs
lexer ('-':cs) = Minus : lexer cs
lexer ('*':cs) = Times : lexer cs
lexer ('/':cs) = Divide : lexer cs

-- arithmetic comparisons
lexer ('>':'=':cs) = GtE : lexer cs
lexer ('<':'=':cs) = LtE : lexer cs
lexer ('=':cs) = Eq : lexer cs
lexer ('>':cs) = Gt : lexer cs
lexer ('<':cs) = Lt : lexer cs

-- braces, parentheses
lexer ('(':cs) = LParen : lexer cs
lexer (')':cs) = RParen : lexer cs
lexer ('{':cs) = LBrace : lexer cs
lexer ('}':cs) = RBrace : lexer cs

-- words
lexer (c:cs) | isAlpha c = let
  (word, rest) = span isAlpha (c:cs)
    in captureKwd word : lexer rest

-- digits
lexer (c:cs) | isDigit c = let
  (numString, rest) = span isDigit (c:cs)
    in Num (read numString) : lexer rest

-- remove comments
lexer (';':';':cs) = lexer (dropWhile (/= '\n') cs)

-- anything not handled
lexer _ = []

captureKwd :: String -> Token
captureKwd "if" = Kwd If
captureKwd "run" = Kwd Run
captureKwd "else" = Kwd Else
captureKwd "while" = Kwd While
captureKwd "ret" = Kwd Ret
captureKwd w = Identifier w