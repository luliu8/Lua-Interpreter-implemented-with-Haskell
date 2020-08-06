
module PropertyTests
  ( module ExpGens
  , anyExpVal_prop
  , anyExpEnvVal_prop
  )
where

import Test.QuickCheck
import qualified Data.HashMap.Strict as H
import Control.Monad.State
import Lua.Eval

import ExpGens


-- classify :: Bool -> String -> Property -> Property
-- counterexample :: Testable prop => String -> prop -> Property
-- Adds the given string to the counterexample if the property fails.

anyExpVal_prop :: ExpValUnit -> Property
anyExpVal_prop (ExpValUnit e v)
    = showFailure $ actualResult === v
  where
    (actualResult, _) = runState (eval e) H.empty
    showFailure = counterexample $ "eval failed on"
                  ++"\nexpression: '"++show e
                  ++"'.\nExpected: '"++show v
                  ++"'\nbut got: '"++show actualResult++"'"



anyExpEnvVal_prop :: ExpEnvValUnit -> Property
anyExpEnvVal_prop (ExpEnvValUnit e env v)
    = showFailure $ actualResult === v
  where
    (actualResult,_) = runState (eval e) env
    showFailure = counterexample $ "eval failed on"
                  ++"\nexpression: '"++show e
                  ++"'\nin environment: '"++show env
                  ++"'.\nExpected: '"++show v
                  ++"'\nbut got: '"++show actualResult++"'"



