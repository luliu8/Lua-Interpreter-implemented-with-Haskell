module UnitTests where
import Data.HashMap.Strict as H

import Test.Tasty.HUnit
import PropertyTests (parseExecTest)

--assertEqual name expect actual



-- Operator expressions 
intOpExpUnitTests :: [(String, Assertion)]
intOpExpUnitTests =
  [ ( "^"
    , assertEqual ""
      "8" (parseExecTest "print (2^3)")
    )
  , ( "unop-"
    , assertEqual ""
      "-10" (parseExecTest "print (-10)")
    )
  , ( "*"
    , assertEqual ""
      "12" (parseExecTest "print (3*4)")
    )
  , ( "/"
    , assertEqual ""
      "2" (parseExecTest "print (8/4)")
    )
  , ( "%"
    , assertEqual ""
      "1" (parseExecTest "print (7%2)")
    )
  , ( "+"
    , assertEqual ""
      "12" (parseExecTest "print (3+9)")
    )
  , ( "-"
    , assertEqual ""
      "1" (parseExecTest "print (10-9)")
    )
  ]
 