module ExpGens where

import Test.QuickCheck

import qualified Data.HashMap.Strict as H
import Lua.Core

--- eval
--- ----

data ExpValUnit = ExpValUnit Exp Val
  deriving(Show, Eq) 

data ExpEnvValUnit = ExpEnvValUnit Exp Env Val
  deriving(Show, Eq)

--- ### Constants

arbConstExp :: Gen ExpValUnit
arbConstExp = oneof [ (return $ ExpValUnit NilExp NilVal)
                    , (ExpValUnit <$> IntExp  <*>  IntVal) <$> arbitrary
                    , (ExpValUnit <$> BoolExp <*> BoolVal) <$> arbitrary 
                    , (ExpValUnit <$> StrExp  <*>  StrVal) <$> arbitrary ]

--- ### Variables
arbParam :: Gen String
arbParam = elements $ map (:"") ['a'..'z']


arbTable :: Gen Val 
arbTable = do
  ks <- resize 3 $ listOf arbValnoNil -- NilVal can't be used as keys 
  vs <- listOf arbVal
  return $ TableVal $ H.fromList $ zip ks vs 



arbVal :: Gen Val
arbVal = frequency [(4, arbValnoNil), (1, return NilVal)] 

arbValnoNil :: Gen Val 
arbValnoNil = oneof [ IntVal <$> arbitrary
                    , BoolVal <$> arbitrary
                    , StrVal <$> arbitrary
                    , arbTable]

arbEnv :: Gen Env
arbEnv = do
  ps <- resize 3 $ listOf arbParam
  vs <- listOf arbVal
  return $ H.fromList $ zip ps vs

arbVarExp :: Gen ExpEnvValUnit
arbVarExp = do
    var <- arbParam
    env <- arbEnv
    val <- arbVal
    frequency
        [ ( 4, return $ ExpEnvValUnit (VarExp var) (H.insert var val env) val )
        , ( 1, return $ ExpEnvValUnit (VarExp var) (H.delete var env) NilVal )
        ]