import Test.Tasty ( defaultMain, testGroup, TestTree )
import Test.Tasty.HUnit ( Assertion, assertEqual, testCase )
import Test.Tasty.QuickCheck ( testProperty )
import Test.QuickCheck
import Test.QuickCheck.IO

import Data.Foldable
import Data.HashMap.Strict as H ( HashMap, empty, singleton, toList, fromList
                                , insert, lookup, union, delete, null)

import PropertyTests


main :: IO ()    
main = putStrLn "" >> defaultMain tests

tests = testGroup "Property Tests"
  [ testGroup "eval Function"
    [ testProperty
      "Constant Expressions"
      $ forAll arbConstExp anyExpVal_prop
    ]
  ]
