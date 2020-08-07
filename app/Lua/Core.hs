module Lua.Core where

import Prelude hiding (lookup)
import Data.HashMap.Strict as H (HashMap, insert, lookup, empty, fromList)
import Data.Typeable
import Control.Monad.State
import Control.Monad.Except
import Data.Maybe

import GHC.Generics (Generic)
import Data.Hashable 


--- ### Environment
type Env = H.HashMap String Val

--- ### Values
-- primitive values of lua 
-- 5 of 8 basic types in Lua: nil, boolean, number, string, table.
--  omitted function, userdata, thread 

-- Table
-- key can contain values of all types (except nil). 
-- Any key associated to the value nil is not considered 
-- part of the table. Conversely, any key that is not part of a 
-- table has an associated value nil.
type Table = H.HashMap Val Val 

data Val = NilVal  -- cannot be used as table index 
     | BoolVal Bool
     | IntVal Int 
     | StrVal String   
     | TableVal Table  
    deriving (Typeable, Generic, Eq)

instance Hashable Val 


instance Show Val where
  show NilVal = "nil"
  show (TableVal table) = "{" ++ (show table) ++ "}"  
  show (BoolVal b) = if b == True then "true" else "false"
  show (IntVal i) = show i 
  show (StrVal s) =  s 
  
  

--- ### Expressions
-- program expressions that can be evaluated to Val

data Exp = NilExp
     | IntExp Int
     | BoolExp Bool
     | StrExp String -- literal string, simply evaluate to StrVal 
     | VarExp String  -- before first assignment to a variable, value is nil 
     | UnopExp String Exp    
     | BinopExp String Exp Exp 
     | TableConstructor [(Exp,Exp)]  -- evaluate to a TableVal     
     | TableLookUpExp Exp Exp -- take a VarExp that evaluate to a TableVal and key expression
  deriving (Eq, Show)
   

--- ### Statements

data Stmt = AssignStmt [Exp] [Exp] -- variable assignment, support multiple assignment
          | TableAssignStmt Exp Exp Exp -- t[key] = val 
          | PrintStmt Exp -- printing
          | SeqStmt [Stmt] -- a sequence of statements to be executed 
          | QuitStmt 
    deriving (Eq, Show)

 
type EvalState a = State Env a
