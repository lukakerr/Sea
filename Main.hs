import Sea.Lexer
import Sea.Parser
import Sea.Syntax
import Sea.Evaluator

import System.IO
import System.Exit
import System.Environment

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> error "Must supply a .sea file"
    ["repl"] -> repl
    [file] -> do
      contents <- readFile file
      evaluatr contents
    [file, arg] -> do
      contents <- readFile file
      case arg of
        "--lexer" -> lexr contents
        "--parser" -> parsr contents
        _ -> evaluatr contents
    _ -> error "Currently only accepts a single file"

-- run an interactive repl
repl :: IO ()
repl = do
  putStr "Sea> "
  hFlush stdout
  line <- getLine
  case line of
    ":q" -> exitSuccess
    _ -> evaluatr line
  repl

-- run the lexer
lexr :: String -> IO ()
lexr = print . lexer

-- run the parser
parsr :: String -> IO ()
parsr = print . parser . lexer

-- run the evaluator
evaluatr :: String -> IO ()
evaluatr = putStrLn . showValue . evaluator . parser . lexer
