import Test.Tasty ( defaultMain, testGroup, TestTree )
import Test.Tasty.QuickCheck ( testProperty )
import Test.QuickCheck

import Data.Foldable
import Data.HashMap.Strict as H ( HashMap, empty, singleton, toList, fromList
                                , insert, lookup, union, delete, null)

import PropertyTests


main :: IO ()    
main = putStrLn "" >> defaultMain tests


tests = testGroup "Property Tests"
  [ testProperty
    "Constant Expressions"
    $ forAll arbConst anyCodeResult_prop
  ]

{-
  , testProperty
    "Assignment and Variable Expressions"
    $ forAll arbAssignVar anyCodeResult_prop
-}



{-
Constant Expressions
Assignment Statement and Variable Expressions
Binary Operation Expressions
Unary Operation Expressions
Table manipulations"
-}

{-
tests = testGroup "Property Tests"
  [ testProperty
    "Constant Expressions"
    $ forAll arbConstExp anyExpVal_prop

  , testProperty
    "Variable Expressions"
    $ forAll arbVarExp anyExpEnvVal_prop
]

-}