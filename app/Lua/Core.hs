{-# LANGUAGE FlexibleContexts #-}

module Lua.Core where

import Prelude hiding (lookup)
import Data.HashMap.Strict as H (HashMap, insert, lookup, empty, fromList)
import Data.Typeable
import Control.Monad.State
import Control.Monad.Except
import Data.Maybe


--- ### Environment
type Env = H.HashMap String Val

--- ### Values
-- primitive values of lua 
-- 6 basic types in Lua, omitted userdata, thread 
-- values calculated during evaluation

-- represent dictionary 
-- key can contain values of all types (except nil). 
-- Any key associated to the value nil is not considered 
-- part of the table. Conversely, any key that is not part of a 
-- table has an associated value nil.
type Table = H.HashMap Val Val 
data Val = NilVal 
     | BoolVal Bool
     | IntVal Int 
     | StrVal String    
     {-
     | FunVal [String] BlockStmt Env -- parameters, function body, closure env
     -}
     | TableVal Table   
     | Void          
    deriving (Eq, Typeable)

instance Show Val where
  show NilVal = "nil"
  show Void = ""
  show (TableVal table) = "{" ++ (show table) ++ "}"
  show (BoolVal b) = show b
  show (IntVal i) = show i 
  show (StrVal s) = show s 
  
  --show (_ i) = show i  -- Bool, Int, String
  {-
    show (FunVal name params body env) = "<" ++ show name   ++ ", "
                         ++ show params   ++
                                           ++ show body ++ ", "
                                         ++ show env  ++ ">"
  -}

--- ### Expressions
-- program expressions that can be evaluated to Val

{-
Unop:
unary minus 
unary bitwise NOT
unary logical not
unary length operator # 

Binop: 
arithmetic operators
bitwise operators 
relational operators
logical operators 
concatenation operator '..'
-}
data Exp = NilExp
     | IntExp Int
     | BoolExp Bool
     | StrExp String -- literal string, simply evaluate to StrVal 
     {-
     | FunExp String [String] BlockStmt  -- function definition: name, parameters, function body. evaluatae to FunVal, also add to Penv, so Callstmt can use it(has side effects)
     | AppExp Exp [Exp] -- function call: todo: how to obtain return value from return statement 
     -}
     | Unop String Exp    
     | Binop String Exp Exp 
     | VarExp String  -- lookup variable name in environment 
              -- before first assignment to a variable, value is nil 
     | TableConstructor [(Exp,Exp)]  -- evaluate to a TableVal     
     | TableLookUpExp Table Exp -- todo: lookup key in table 
  deriving (Show, Eq)

--- ### Statements
-- program statements that can be executed
-- A statement is an operation intended to yield a side effect.
-- The condition expression of a control structure can return 
-- any value. Both false and nil test false. All values different 
-- from nil and false test true. In particular, the number 0 and the empty string also test true.
data Stmt = AssignStmt [String] [Exp] -- variable assignment, support multiple assignment
          | AssignLocalStmt [String] [Exp] -- local varable declaration. scope within the block 
          | IfStmt [(Exp, Stmt)] Stmt -- conditional statements
          | BlockStmt [Stmt] -- sequence statements, act like semi-colon 
          | WhileStmt Exp Stmt -- while exp do block end 
          | RepeatStmt Stmt Exp -- repeat block until exp， condition exp can refer to local variables declared inside the loop block 
          | ForNumStmt String Exp Exp Exp -- numerical for loop 
          {-
          -- procedure and call is equivalent to using function as statement. i.e. discard return value 
          | ProcedureStmt String [String] Stmt -- define precedure/function, add to Penv 
          | ReturnStmt [Exp]
          | CallStmt String [Exp] -- call function by name, with arguments. discard return value
                    -}
          | PrintStmt Exp -- printing
    deriving (Show, Eq)

--- ### Diagnostic
-- todo: 还没有改 
data Diagnostic = UnexpectedArgs [Val]
                | NotFuncError Val
                | UndefSymbolError String
                | NotArgumentList Val
                | InvalidSpecialForm String Val
                | CannotApply Val [Val]
                | InvalidExpression Val
                | NotASymbol Val
                | NotAListOfTwo Val
                | UnquoteNotInQuasiquote Val
                | Unimplemented String
instance Show Diagnostic where
  show (UnexpectedArgs actual) =
    "Error: Unexpected arguments or wrong number of arguments (" ++ unwords (map show actual) ++ ")"
  show (NotFuncError val) =
    "Error: " ++ show val ++ " is not a function"
  show (UndefSymbolError name) =
    "Error: Symbol " ++ name ++ " is undefined"
  show (NotArgumentList val) =
    "Error: Expecting an argument list, but found " ++ show val
  show (InvalidSpecialForm f val) =
    "Error: Invalid pattern in special form `" ++ f ++ "`: " ++ show val
  show (CannotApply val args) =
    "Error: Cannot apply " ++ show val ++ " on argument list (" ++ unwords (map show args) ++ ")"
  show (InvalidExpression val) =
    "Error: Invalid expression: " ++ show val
  show (NotASymbol val) =
    "Error: Not a symbol: " ++ show val
  show (NotAListOfTwo val) =
    "Error: Not a list of two elements: " ++ show val
  show (UnquoteNotInQuasiquote val) =
    "Error: `unquote` not in a `quasiquote` context: " ++ show val
  show (Unimplemented feature) =
    "Error: " ++ feature ++ " is not implemented. You should implement it first!"

-- state may need to be (Penv and Env)
-- can i put both in the same hashmap ? 
type EvalState a = StateT Env (Except Diagnostic) a
