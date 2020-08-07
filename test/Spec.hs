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
      , testGroup "Bool Operator Expressions"
        $ buildTestCase boolOpExpUnitTests
      , testGroup "String Operator Expressions"
        $ buildTestCase strOpExpUnitTests
      , testGroup "Logical Operator Expressions"
        $ buildTestCase logicOpExpUnitTests
      ]
  , testGroup "Assignment and Variable Expressions"
        $ buildTestCase assignVarUnitTests
  , testGroup "Table Manipulations"
        $ buildTestCase tableUnitTests
  ]
 
