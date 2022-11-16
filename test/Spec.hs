import Ast
import EvalExpr
import Errors
import TypeCheck

import Test.Hspec.Expectations (shouldBe, shouldThrow, errorCall)
import Test.Hspec.Core.Runner (hspec)
import Test.Hspec.Core.Spec (describe, it, context)
import Control.Exception.Base (evaluate)

main :: IO ()
main = hspec $ do
  describe "EvalExpr" $ do 
    describe "evalBinOp" $ do
      context "Add" $ do 
        let sIntVal1 = VInt (-1) Signed
        let sIntVal2 = VInt (-2) Signed
        let uIntVal1 = VInt 1 Unsigned 
        let uIntVal2 = VInt 2 Unsigned
        let charVal  = VChar 'a'
        let boolVal  = VBool True
        let voidVal  = VVoid
        let arrVal   = VArray [VInt 3 Signed] (TInt Signed)
        it "should add two signed integers" $ do
          evalBinOp Add sIntVal1 sIntVal2 `shouldBe` VInt (-3) Signed
        it "should add a signed integer and an unsigned integer" $ do
          evalBinOp Add sIntVal1 uIntVal1 `shouldBe` VInt 0 Signed
        it "should add a signed integer and a char" $ do
          evalBinOp Add sIntVal1 charVal `shouldBe` VInt 96 Signed
        it "should add a signed integer and a bool" $ do
          evalBinOp Add sIntVal1 boolVal `shouldBe` VInt 0 Signed
        it "should throw an exception when adding a signed integer and a void" $ do
          evaluate (evalBinOp Add sIntVal1 voidVal) `shouldThrow` errorCall (cannotConvertException TVoid (TInt Signed))
        it "should throw an exception when adding a signed integer and an array" $ do
          evaluate (evalBinOp Add sIntVal1 arrVal) `shouldThrow` errorCall (cannotConvertException (getType arrVal) (TInt Signed))