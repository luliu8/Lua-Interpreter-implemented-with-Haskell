{-# LANGUAGE FlexibleContexts, LambdaCase #-}

module Lua.Eval where

import Lua.Core

import Prelude hiding (lookup)
import qualified Data.HashMap.Strict as H (HashMap, insert, lookup, empty, fromList, union)
import Control.Monad.State
import Control.Monad.Except



-- ### The monadic evaluator
eval :: Exp -> EvalState Val
eval NilExp = return NilVal 
eval (IntExp i) = return $ IntVal i
eval (BoolExp b) = return $ BoolVal b 
eval (StrExp s) = return $ StrVal s 


exec :: Stmt -> EvalState String 
exec (PrintStmt e) = do v <- eval e 
                        return (show v)