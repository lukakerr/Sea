import Sea.Lexer
import Sea.Parser
import Sea.Syntax
import Sea.Program
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
    _ ->
      let main = "fn main {} ( " ++ line ++ " )" in
        evaluatr main
  repl

-- run the lexer
lexr :: String -> IO ()
lexr s = print $ extractTokens $ lexer (s, 1)

-- run the parser
parsr :: String -> IO ()
parsr s = case parser $ lexer (s, 1) of
  Right prog -> print prog
  Left  ex   -> putStrLn $ showException ex

-- run the evaluator
evaluatr :: String -> IO ()
evaluatr s = case parser $ lexer (s, 1) of
  Right prog -> putStrLn $ showValue $ evaluator prog
  Left  ex   -> putStrLn $ showException ex
