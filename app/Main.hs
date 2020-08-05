module Main where
import Lua.Core
import Lua.Parse
import Lua.Eval
import Lua.Runtime

import Prelude hiding (lookup)
import System.IO (hFlush, hPutStr, hPutStrLn, hGetLine, stdin, stdout)
import Text.ParserCombinators.Parsec hiding (Parser, State)
import Control.Monad
import Control.Monad.State
import Control.Monad.Except
import qualified Data.HashMap.Strict as H


--- ### REPL

printLn :: String -> IO ()
printLn str = hPutStrLn stdout str >> hFlush stdout

repl :: Env -> IO ()
repl env = do
  putStr "Lua> "
  hFlush stdout
  l <- getLine                                        -- Read
  case parse stmt "stdin" l of                  -- Parse
    Left err       -> print err  -- Diagnostics
    Right QuitStmt -> printLn "bye"                        
    Right s        -> printLn output
                      repl new_env
                      where (output, new_env) = runStateT (exec s) env

main :: IO ()
main = repl H.empty
