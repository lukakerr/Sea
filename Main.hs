import Sea.Lexer
import Sea.Parser
import Sea.Syntax
import Sea.Evaluator
import System.Environment

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> error "Must supply a .sea file"
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

lexr :: String -> IO ()
lexr = print . lexer

parsr :: String -> IO ()
parsr = print . parser . lexer

evaluatr :: String -> IO ()
evaluatr = putStrLn . id showValue . evaluator . parser . lexer
