
module Lua.Runtime where

import Lua.Core


import qualified Data.HashMap.Strict as H
import Control.Monad.State
import Control.Monad.Except
import Data.Foldable


lowerBool :: Val -> Bool
lowerBool (BoolVal b) = b

lowerInt :: Val -> Int
lowerInt (IntVal i) = i

lowerStr :: Val -> String
lowerStr (StrVal s) = s 

--- ### Runtime
liftIntBinop :: (Int-> Int-> Int) -> Val -> Val -> EvalState Val -- PrimBinop (Val -> Val -> EvalState Val) 
liftIntBinop f = p where
    p v1 v2 = return $ IntVal $ f (lowerInt v1) (lowerInt v2)

liftIntUnop :: (Int-> Int)-> Val -> EvalState Val -- PrimUnop (Val -> EvalState Val) 
liftIntUnop f = p where
    p v = return $ IntVal $ f $ lowerInt v 

liftStrUnop :: (String -> Int) -> Val -> EvalState Val 
liftStrUnop f = p where 
    p v = return $ IntVal $ f $ lowerStr v  

liftBoolBinop :: (Int-> Int->Bool)->Val -> Val -> EvalState Val
liftBoolBinop f = p where
    p v1 v2 = return $ BoolVal $ f (lowerInt v1) (lowerInt v2)

to_str :: Val -> String 
to_str (IntVal i) = show i
to_str (StrVal s) = s 

luaConcat :: Val -> Val -> EvalState Val
luaConcat v1 v2 = return $ StrVal $ to_str(v1) ++ to_str(v2)

 

{- Eq and InEq 
If the types are different, then the result is false. 
Otherwise, the values of the operands are compared. 
Strings are equal if they have the same byte content. 
Numbers are equal if they denote the same mathematical value. 

Tables are compared by reference. (not supported here)
-}
luaEq :: Val -> Val -> EvalState Val 
luaEq NilVal NilVal = return $ BoolVal True
luaEq (IntVal i1) (IntVal i2) = return $ BoolVal (i1 == i2)
luaEq (BoolVal b1) (BoolVal b2) = return $ BoolVal (b1 == b2)
luaEq (StrVal s1) (StrVal s2) = return $ BoolVal (s1 == s2)
luaEq _ _ = return $ BoolVal False 

luaIneq :: Val -> Val -> EvalState Val 
luaIneq v1 v2 = do v <- luaEq v1 v2 
                   return $ BoolVal $ not $ lowerBool v


{-
logical operators not, and, or (arguments can be any value type)
all logical operators consider both false and nil as false and anything else as true.
-}
--  The negation operator not always returns false or true. 
luaNot :: Val -> EvalState Val 
luaNot NilVal = return $ BoolVal True
luaNot (BoolVal False) = return $ BoolVal True
luaNot _ = return $ BoolVal False
{-
The conjunction operator and (arguments can be any value type)
returns its first argument if this value is false or nil; otherwise, and 
 returns its second argument. 
-}
luaAnd :: Val -> Val -> EvalState Val 
luaAnd NilVal _ = return NilVal
luaAnd (BoolVal False) _ = return $ BoolVal False 
luaAnd _ v2 = return v2 


{-
The disjunction operator or (arguments can be any value type)
returns its first argument 
 if this value is different from nil and false; 
 otherwise, or returns its second argument
-}
luaOr :: Val -> Val -> EvalState Val 
luaOr NilVal v2 = return v2
luaOr (BoolVal False) v2 = return v2 
luaOr v1 _ = return v1 

data PrimFunc = PrimUnop (Val -> EvalState Val) -- cannot be used as table index 
                | PrimBinop (Val -> Val -> EvalState Val) -- cannot be used as table index 

type PrimFuncEnv = H.HashMap String PrimFunc 

intOps :: [(String, PrimFunc)]
intOps = [ ("^", PrimBinop $ liftIntBinop (^))
         , ("unop-", PrimUnop $ liftIntUnop negate )
         , ("*", PrimBinop $ liftIntBinop (*))
         , ("/", PrimBinop $ liftIntBinop div)
         , ("%", PrimBinop $ liftIntBinop mod)
         , ("+", PrimBinop $ liftIntBinop (+))
         , ("-", PrimBinop $ liftIntBinop (-))]

boolOps ::[(String, PrimFunc)]
boolOps = [ ("<", PrimBinop $ liftBoolBinop (<))
          , (">", PrimBinop $ liftBoolBinop (>))
          , ("<=", PrimBinop $ liftBoolBinop (<=))
          , (">=", PrimBinop $ liftBoolBinop (>=))]
                   
strOps :: [(String, PrimFunc)]
strOps = [ ("unop#", PrimUnop $ liftStrUnop length)
         , ("..", PrimBinop luaConcat)]

logicalEqOps :: [(String, PrimFunc)]
logicalEqOps = [("unopnot", PrimUnop luaNot)
               , ("~=", PrimBinop luaIneq)
               , ("==", PrimBinop luaEq)
               , ("and", PrimBinop luaAnd)
               , ("or", PrimBinop luaOr)]


runtime :: PrimFuncEnv
-- map (String : PrimBinop/PrimUnop)
runtime = H.fromList $ intOps ++ boolOps ++ strOps ++ logicalEqOps
                     


                     
