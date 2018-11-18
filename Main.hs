import Sea.Lexer
import Sea.Parser
import Sea.Syntax
import Sea.Evaluator
import System.Environment
import System.IO
import System.Exit

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

repl :: IO ()
repl = do
  putStr "Sea >> "
  hFlush stdout
  line <- getLine
  if line == ":q" then exitSuccess else do
    evaluatr line
    repl

lexr :: String -> IO ()
lexr = print . lexer

parsr :: String -> IO ()
parsr = print . parser . lexer

evaluatr :: String -> IO ()
evaluatr = putStrLn . id showValue . evaluator . parser . lexer
