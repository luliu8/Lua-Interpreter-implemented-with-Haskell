import Test.Tasty ( defaultMain, testGroup, TestTree )
import Test.Tasty.HUnit ( Assertion, assertEqual, testCase )
import Test.Tasty.QuickCheck ( testProperty )
import Test.QuickCheck

import Data.Foldable
import Data.HashMap.Strict as H ( HashMap, empty, singleton, toList, fromList
                                , insert, lookup, union, delete, null)
import UnitTests
import PropertyTests


main :: IO ()    
main = putStrLn "" >> defaultMain tests

tests = testGroup "All Tests"
  [ unitTests, propTests] 


propTests :: TestTree
propTests = testGroup "Property Tests"
  [ testProperty "Constant Expressions"
    $ forAll arbConst anyCodeResult_prop
  ]

buildTestCase :: [(String, Assertion)] -> [TestTree]
buildTestCase = map $ uncurry testCase

unitTests :: TestTree
unitTests = testGroup "Unit Tests"
  [ testGroup "Operator Expressions"
      [ testGroup "Int Operator Expressions"
        $ buildTestCase intOpExpUnitTests
      ]
  ]
  {-

 , testGroup "Bool Operator Expressions"
        $ buildTestCase boolOpExpUnitTests
      , testGroup "String Operator Expressions"
        $ buildTestCase compOpExpUnitTests
      , testGroup "Logical Operator Expressions"
        $ buildTestCase compOpExpUnitTests
   , testGroup "Assignment and Variable expression"
   	  [ testGroup "Int Operator Expressions"
        $ buildTestCase intOpExpUnitTests
      , testGroup "Bool Operator Expressions"
        $ buildTestCase boolOpExpUnitTests
      , testGroup "Comp Operator Expressions"
        $ buildTestCase compOpExpUnitTests
      ]
  -}


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