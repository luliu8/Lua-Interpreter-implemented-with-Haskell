
module PropertyTests
  ( module ExpGens
  , anyExpVal_prop
  )
where

import Test.QuickCheck
import qualified Data.HashMap.Strict as H

import Lua.Eval

import ExpGens


isException :: Val -> Bool
isException (ExnVal _) = True
isException         _  = False

-- classify :: Bool -> String -> Property -> Property
-- counterexample :: Testable prop => String -> prop -> Property
-- Adds the given string to the counterexample if the property fails.

anyExpVal_prop :: ExpValUnit -> Property
anyExpVal_prop (ExpValUnit e v)
    = classify (isException v) "Exception"
      $ showFailure $ actualResult === v
  where
    actualResult, _ = runState (eval e) H.empty
    showFailure = counterexample $ "eval failed on"
                  ++"\nexpression: '"++show e
                  ++"'.\nExpected: '"++show v
                  ++"'\nbut got: '"++show actualResult++"'"