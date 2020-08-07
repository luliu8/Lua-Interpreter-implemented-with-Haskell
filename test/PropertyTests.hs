
module PropertyTests where

import Test.QuickCheck
import qualified Data.HashMap.Strict as H
import Control.Monad.State
import Lua.Eval
import Lua.Parse
import Lua.Runtime 

import Text.ParserCombinators.Parsec hiding (Parser, State)

-- parse a line of code and execute 
parseExecTest :: String -> String 
parseExecTest l = case parse stmt "test" l of                 
    Left err       -> show err 
    Right s        -> let (result, _) = runState (exec s) H.empty
                      in result 

data CodeResultUnit = CodeResultUnit String String 
  deriving(Show, Eq) 

anyCodeResult_prop :: CodeResultUnit -> Property
anyCodeResult_prop (CodeResultUnit code result)
    = showFailure $ actualResult === result
        where
          actualResult = parseExecTest code 
          showFailure = counterexample $ "failed on"
                        ++"\ncode: '"++show code
                        ++"'.\nExpected: '"++show result
                        ++"'\nbut got: '"++show actualResult++"'"



-- ### constant expressions : nil, true/false, int, String
-- generate random code string such as print (true), print ("abc")
 
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

