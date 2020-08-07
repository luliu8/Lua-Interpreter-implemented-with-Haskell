
module PropertyTests where

import Test.QuickCheck
import qualified Data.HashMap.Strict as H
import Control.Monad.State
import Lua.Eval
import Lua.Parse

-- import System.Process
import Text.ParserCombinators.Parsec hiding (Parser, State)

{-
main = do output <- readProcess "lua" ["-e", "do a = 10; print (a) end"] ""
          print $output
-}

data CodeResultUnit = CodeResultUnit String String 
  deriving(Show, Eq) 


-- the output string from my exec should match that of reference lua interpreter 
anyCodeResult_prop :: CodeResultUnit -> Property
anyCodeResult_prop (CodeResultUnit code result)
    = showFailure $ actualResult === result
        where
          actualResult = case parse stmt "" code of 
                            Left err -> show err 
                            Right s  -> let (output, _) = runState (exec s) H.empty
                                        in output 
          showFailure = counterexample $ "failed on"
                        ++"\ncode: '"++show code
                        ++"'.\nExpected: '"++show result
                        ++"'\nbut got: '"++show actualResult++"'"




-- ### constant expressions 
-- e.g. print ("abc") , print (true), print(3) 

--do print ("abc"); print(123); print(true) end 


-- constant expressions : nil, true/false, int, String 
arbInt :: Gen CodeResultUnit 
arbInt = do int <- arbitrary :: Gen Int
            let code = "print ("++ show int ++ ")"
            return $ CodeResultUnit code $ show int 

arbString :: Gen CodeResultUnit 
arbString = do s <- elements $ map (:"") ['a'..'z']
               let code = "print ("++  show s ++ ")"
               return $ CodeResultUnit code $ s 

arbNilBool :: Gen CodeResultUnit 
arbNilBool = do x <- elements ["nil", "true", "false"] 
                let code = "print ("++ x ++ ")"
                return $ CodeResultUnit code x 

arbConst :: Gen CodeResultUnit
arbConst = oneof [arbInt, arbString, arbNilBool]

--- ### assignment statement and variable expressions 
-- e.g. do a,b,c = 10,20,30; print(a); print(b); print (c) end
-- arbAssignVar :: Gen CodeResultUnit




-- ### Binary operations 



--- ### Unary operations 
-- print (-10)



--- ### Table manipulations 
-- e.g. do t = {[“x”]= 10}; t[3] = 111; print (t[“x”]) end  





{-
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


-}
