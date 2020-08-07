module UnitTests where
import Data.HashMap.Strict as H

import Test.Tasty.HUnit
import PropertyTests (parseExecTest)

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
  , ( "int op mix"
    , assertEqual ""
      "1" (parseExecTest "print ((-1*2^3+5-10/2)%3)")
    )  ]
 
boolOpExpUnitTests :: [(String, Assertion)]
boolOpExpUnitTests =
  [ ( "<"
    , assertEqual ""
      "true" (parseExecTest "print (2<3)")
    )
  , ( ">"
    , assertEqual ""
      "false" (parseExecTest "print (10>100)")
    )
  , ( "<="
    , assertEqual ""
      "false" (parseExecTest "print (3<=2)")
    )
  , ( ">="
    , assertEqual ""
      "false" (parseExecTest "print (1>=10)")
    )
  , ( "int bool op mix"
    , assertEqual ""
      "true" (parseExecTest "print (4*(-5)/2+10 < 10)")
    )
  ]

strOpExpUnitTests :: [(String, Assertion)]
strOpExpUnitTests =
  [ ( ".."
    , assertEqual ""
      "abcdef" (parseExecTest "print (\"abc\"..\"def\")")
    )
  , ( "unop#"
    , assertEqual ""
      "7" (parseExecTest "print (#\"abcdefg\")")
    )
  , ( "str op mix"
    , assertEqual ""
      "6" (parseExecTest "print (#(\"abc\"..\"def\"))")
    )
  , ( "int bool str op mix "
    , assertEqual ""
      "true" (parseExecTest "print (3*4 > #(\"abc\"..\"def\"))")
    )
  ]

logicOpExpUnitTests :: [(String, Assertion)]
logicOpExpUnitTests =
  [ ( "unopnot"
    , assertEqual ""
      "true\ntrue\nfalse\nfalse\n" 
      (parseExecTest "do print (not false); print (not nil); print (not 2); print (not \"abc\") end")
    )
  , ( "and"
    , assertEqual ""
      "false\nabc\n" 
      (parseExecTest "do print (false and true); print(99 and \"abc\") end")
    )
  , ( "or"
    , assertEqual ""
      "99\nnil\nany thing\n" (parseExecTest "do print (99 or nil); print (nil and 2); print (false or \"any thing\") end")
    )
  , ( "~="
    , assertEqual ""
      "true\ntrue\n" (parseExecTest "do print (true ~= 34); print (\"abc\" ~= false) end")
    )
  , ( "=="
    , assertEqual ""
      "true\ntrue\nfalse\n" (parseExecTest "do print (true == true); print (nil == nil); print (3 == true) end")
    )
  , ( "logic mix"
    , assertEqual ""
      "true\n99\n" (parseExecTest "do print (3 and false or nil ~= 3); print (99 or false == false and nil) end")
    )
  ]


assignVarUnitTests :: [(String, Assertion)]
assignVarUnitTests =
  [ ( "single assignment"
    , assertEqual ""
      "4\nfalse\nnil\n99\n" 
      (parseExecTest "do a = 1+3; b = 33 and false; print (a); print (b); b = 99; print (c); print(b) end")
    )
  , ( "multiple assignment"
    , assertEqual ""
      "nil\n16\nabc\n" 
      (parseExecTest "do a,b,c = 3*5+1, \"abc\" ; print (c); print(a); print(b) end")
    )
  ]

tableUnitTests :: [(String, Assertion)]
tableUnitTests =
  [ ( "table constructor"
    , assertEqual ""
      "true\nnil\n{fromList [(3,true),(1,99),(x,abc)]}\n" 
      (parseExecTest "do t = {[1] = 99, [\"x\"]=\"abc\", [#\"key\"]=true}; print (t[3]);print(t[0]);print (t) end")
    ) 

  , ( "table assignment and lookup"
    , assertEqual ""
      "99\nnil\n{fromList [(1,99),(x,true)]}\n" 
      (parseExecTest "do t = {}; t[1] = 99; t[\"x\"] = true; print (t[1]); print (t[0]); print (t) end")
    )
  ]






