module ExpGens where

import Test.QuickCheck

import qualified Data.HashMap.Strict as H
import Lua.Core

--- eval
--- ----

-- Expression x Value Unit test pairs
data ExpValUnit = ExpValUnit Exp Val
  deriving(Show, Eq) 

--- ### Constants

arbConstExp :: Gen ExpValUnit
-- todo: how to test for NilExp and NilVal?  
arbConstExp = oneof [ (ExpValUnit <$> IntExp  <*>  IntVal) <$> arbitrary
                    , (ExpValUnit <$> BoolExp <*> BoolVal) <$> arbitrary 
                    , (ExpValUnit <$> StrExp  <*>  StrVal) <$> arbitrary ]
