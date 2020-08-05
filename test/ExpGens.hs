module ExpGens where

import Test.QuickCheck

import qualified Data.HashMap.Strict as H

--- eval
--- ----

-- Expression x Value Unit test pairs
data ExpValUnit = ExpValUnit Exp Val
  deriving(Show, Eq) 

--- ### Constants

arbConstExp :: Gen ExpValUnit
arbConstExp = oneof [ (ExpValUnit <$> NilExp  <*>  NilVal) <$> arbitrary
					, (ExpValUnit <$> IntExp  <*>  IntVal) <$> arbitrary
                    , (ExpValUnit <$> BoolExp <*> BoolVal) <$> arbitrary 
                    , (ExpValUnit <$> StrExp  <*>  StrVal) <$> arbitrary ]
