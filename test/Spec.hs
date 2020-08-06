import Test.Tasty ( defaultMain, testGroup, TestTree )
import Test.Tasty.QuickCheck ( testProperty )
import Test.QuickCheck

import Data.Foldable
import Data.HashMap.Strict as H ( HashMap, empty, singleton, toList, fromList
                                , insert, lookup, union, delete, null)

import PropertyTests

main :: IO ()    
main = putStrLn "" >> defaultMain tests

tests = testGroup "end to end Property Tests"
  [ testGroup "eval Function"
    [ testProperty
      "Constant Expressions"
      $ forAll arbConstExp anyExpVal_prop

    , testProperty
      "Variable Expressions"
      $ forAll arbVarExp anyExpEnvVal_prop

    , testProperty
      "Binary Operation Expressions"
      $ forAll (arbIntOpExp 3)  anyExpVal_prop
    ]
  ]