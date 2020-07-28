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
eval (VarExp k) = do env <- get 
                     case H.lookup k env of 
                        Just v  -> return v
                        Nothing -> return NilVal
eval (BinopExp op e1 e2) = do v1 <- eval e1
                              v2 <- eval e2   
                              env <- get
                              case H.lookup op env of 
                                Just (PrimBinop f)  -> f v1 v2
                                -- todo: report wrong operator 
                                Just _              -> return NilVal 
                                -- todo: report error operator doesn't exist 
                                Nothing             -> return NilVal
                    
eval (UnopExp op e) = do v <- eval e
                         env <- get
                         case H.lookup op env of 
                          Just (PrimUnop f)  -> f v
                          -- todo: report wrong operator 
                          Just _              -> return NilVal 
                          -- todo: report error operator doesn't exist 
                          Nothing             -> return NilVal



exec :: Stmt -> EvalState Val 
exec (PrintStmt e) = eval e 
