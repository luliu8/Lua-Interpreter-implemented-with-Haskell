
module Lua.Eval where

import Lua.Core
import Lua.Runtime

import Prelude hiding (lookup)
import qualified Data.HashMap.Strict as H (HashMap, insert, lookup, empty, fromList, union)
import Control.Monad.State
import Control.Monad.Except

evalPair :: (Exp, Exp) -> EvalState (Val,Val)
evalPair (e1, e2) = do v1 <- eval e1
                       v2 <- eval e2 
                       return (v1, v2)

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
                              case H.lookup op runtime of 
                                Just (PrimBinop f)  -> f v1 v2
                                Just _              -> return $ StrVal "invalid operator" 
                                Nothing             -> return $ StrVal "invalid operator" 
                    
eval (UnopExp op e) = do v <- eval e
                         case H.lookup op runtime of 
                          Just (PrimUnop f)  -> f v
                          Just _             -> return $ StrVal "invalid operator" 
                          Nothing            -> return $ StrVal "invalid operator" 




eval (TableConstructor fieldExpList) = do fieldValList <- mapM evalPair fieldExpList
                                          return $ TableVal $ H.fromList fieldValList

eval (TableLookUpExp varExp keyExp) = 
      do keyVal <- eval keyExp --todo: report error when keyVal is NilVal 
         tableVal <- eval varExp 
         case tableVal of 
           TableVal t -> case H.lookup keyVal t of 
                          Just v  -> return v
                          Nothing -> return NilVal
           _          -> return $ StrVal "attempting to index a value that's not a table." 

{-
the list of values is adjusted to the length of the list of variables. 
If there are more values than needed, the excess values are thrown away. 
If there are fewer values than needed, the list is extended with nil's.
-}
myZip:: [Exp] -> [Val] -> [(Exp,Val)]
myZip varList valList = aux varList valList [] 
  where aux [] _ acc = acc  -- more val than needed 
        aux (x:xs) (y:ys) acc =  aux xs ys ((x,y):acc)
        aux (x:xs) [] acc = aux xs [] ((x, NilVal):acc) -- fewer values than needed pad with nil's 


exec :: Stmt -> EvalState String 
exec (PrintStmt e) = do v <- eval e 
                        return $ show v

-- key expression could be VarExp or TableLookUpExp 
exec (AssignStmt keyExpList valExpList) = do
     valList <- mapM eval valExpList 
     let f = (\((VarExp s),v) ->  modify $ H.insert s v)
     mapM f $ myZip keyExpList valList 
     return ""

exec (TableAssignStmt e1@(VarExp tableName) e2 e3) = do 
      oldTableVal <- eval e1 
      case oldTableVal of 
        (TableVal oldTable) -> do keyVal   <- eval e2 
                                  newVal   <- eval e3 
                                  let newTable = H.insert keyVal newVal oldTable
                                  modify $ H.insert tableName (TableVal newTable)
                                  return ""
        _                   -> return "attempting to index a value that's not a table."
                                

