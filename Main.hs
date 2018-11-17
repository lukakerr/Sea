import Sea.Lexer
import System.Environment

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> error "Must supply a .sea file"
    [file] -> do
      contents <- readFile file
      print $ lexer contents
    _ -> error "Currently only accepts a single file"
