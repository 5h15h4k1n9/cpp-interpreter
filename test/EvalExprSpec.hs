module EvalExprSpec where

import Ast
import EvalExpr
import TypeCheck
import Errors

import Test.Hspec.Expectations (shouldBe, shouldThrow, errorCall)
import Test.Hspec.Core.Spec (describe, it, context, Spec)
import Control.Exception.Base (evaluate)

spec :: Spec
spec = do
  describe "evalBinOp" $ do
    context "Arithmetic" $ do
      context "Add" $ do
        it "should add two signed integers" $ do
          let evalOp =  evalBinOp Add (VInt (-1)   Signed  ) (VInt 2      Signed  )
          (evalOp, getType evalOp) `shouldBe` (VInt 1 Signed, TInt Signed)
        it "should add two unsigned integers" $ do
          let evalOp =  evalBinOp Add (VInt 1      Unsigned) (VInt 2      Unsigned)
          (evalOp, getType evalOp) `shouldBe` (VInt 3 Unsigned, TInt Unsigned)
        it "should add a signed integer and an unsigned integer and return unsigned integer #1" $ do
          let evalOp =  evalBinOp Add (VInt (-1)   Signed  ) (VInt 2      Unsigned)
          (evalOp, getType evalOp) `shouldBe` (VInt 1 Unsigned, TInt Unsigned)
        it "should add a signed integer and an unsigned integer and return unsigned integer #2" $ do
          let evalOp =  evalBinOp Add (VInt 1      Unsigned) (VInt (-2)   Signed  )
          (evalOp, getType evalOp) `shouldBe` (VInt 4294967295 Unsigned, TInt Unsigned)
        it "should add a signed integer and an unsigned integer and return unsigned integer #3" $ do
          let evalOp = evalBinOp Add (VInt 76     Unsigned) (VInt (-103) Signed  )
          (evalOp, getType evalOp) `shouldBe` (VInt 4294967269 Unsigned, TInt Unsigned)
        it "should add a signed integer and an unsigned integer and return unsigned integer #4" $ do
          let evalOp = evalBinOp Add (VInt (-76)  Signed  ) (VInt 103    Unsigned)
          (evalOp, getType evalOp) `shouldBe` (VInt 27 Unsigned, TInt Unsigned)
        it "should add a signed integer and a bool and return signed integer" $ do
          let evalOp = evalBinOp Add (VInt (-1)   Signed  ) (VBool True )
          (evalOp, getType evalOp) `shouldBe` (VInt 0 Signed, TInt Signed)
        it "should add a signed integer and a char and return signed integer" $ do
          let evalOp = evalBinOp Add (VInt 1   Signed  ) (VChar 'a'  )
          (evalOp, getType evalOp) `shouldBe` (VInt 98 Signed, TInt Signed)
        it "should add an unsigned integer and a bool and return unsigned integer" $ do
          let evalOp = evalBinOp Add (VInt 1      Unsigned) (VBool True )
          (evalOp, getType evalOp) `shouldBe` (VInt 2 Unsigned, TInt Unsigned)
        it "should add an unsigned integer and a char and return unsigned integer" $ do
          let evalOp = evalBinOp Add (VInt 1      Unsigned) (VChar 'a'  )
          (evalOp, getType evalOp) `shouldBe` (VInt 98 Unsigned, TInt Unsigned)
        it "should add a bool and a bool and return unsigned integer" $ do
          let evalOp = evalBinOp Add (VBool True ) (VBool True )
          (evalOp, getType evalOp) `shouldBe` (VInt 2 Unsigned, TInt Unsigned)
        it "should add a bool and a char and return unsigned integer" $ do
          let evalOp = evalBinOp Add (VBool True ) (VChar 'a'  )
          (evalOp, getType evalOp) `shouldBe` (VInt 98 Unsigned, TInt Unsigned)
        it "should add a char and a char and return unsigned integer" $ do
          let evalOp = evalBinOp Add (VChar 'a'  ) (VChar 'b'  )
          (evalOp, getType evalOp) `shouldBe` (VInt 195 Unsigned, TInt Unsigned)
        it "should throw an error when adding anything with void" $ do
          evaluate (evalBinOp Add VVoid (VInt 1 Signed)) `shouldThrow` errorCall (cannotEvalBinOpException Add VVoid (VInt 1 Signed))
        it "should throw an error when adding anything with array" $ do
          evaluate (evalBinOp Add (VArray 0 [] (TInt Signed)) (VInt 1 Signed)) `shouldThrow` errorCall (cannotEvalBinOpException Add (VArray 0 [] (TInt Signed)) (VInt 1 Signed))

      context "Sub" $ do
        it "should subtract two signed integers" $ do
          let evalOp =  evalBinOp Sub (VInt 1      Signed  ) (VInt 2      Signed  )
          (evalOp, getType evalOp) `shouldBe` (VInt (-1) Signed, TInt Signed)
        it "should subtract two unsigned integers" $ do
          let evalOp =  evalBinOp Sub (VInt 1      Unsigned) (VInt 2      Unsigned)
          (evalOp, getType evalOp) `shouldBe` (VInt 4294967295 Unsigned, TInt Unsigned)
        it "should subtract a signed integer and an unsigned integer and return unsigned integer #1" $ do
          let evalOp =  evalBinOp Sub (VInt (-1)   Signed  ) (VInt 2      Unsigned)
          (evalOp, getType evalOp) `shouldBe` (VInt 4294967293 Unsigned, TInt Unsigned)
        it "should subtract a signed integer and an unsigned integer and return unsigned integer #2" $ do
          let evalOp =  evalBinOp Sub (VInt 1      Unsigned) (VInt (-2)   Signed  )
          (evalOp, getType evalOp) `shouldBe` (VInt 3 Unsigned, TInt Unsigned)
        it "should subtract a signed integer and an unsigned integer and return unsigned integer #3" $ do
          let evalOp = evalBinOp Sub (VInt 76     Unsigned) (VInt (-103) Signed  )
          (evalOp, getType evalOp) `shouldBe` (VInt 179 Unsigned, TInt Unsigned)
        it "should subtract a signed integer and an unsigned integer and return unsigned integer #4" $ do
          let evalOp = evalBinOp Sub (VInt (-76)  Signed  ) (VInt 103    Unsigned)
          (evalOp, getType evalOp) `shouldBe` (VInt 4294967117 Unsigned, TInt Unsigned)
        it "should subtract a signed integer and a bool and return signed integer" $ do
          let evalOp = evalBinOp Sub (VInt (-1)   Signed  ) (VBool True )
          (evalOp, getType evalOp) `shouldBe` (VInt (-2) Signed, TInt Signed)
        it "should subtract a signed integer and a char and return signed integer" $ do
          let evalOp = evalBinOp Sub (VInt 1   Signed  ) (VChar 'a'  )
          (evalOp, getType evalOp) `shouldBe` (VInt (-96) Signed, TInt Signed)
        it "should substract an unsigned integer and a bool and return unsigned integer" $ do
          let evalOp = evalBinOp Sub (VInt 1      Unsigned) (VBool True )
          (evalOp, getType evalOp) `shouldBe` (VInt 0 Unsigned, TInt Unsigned)
        it "should substract an unsigned integer and a char and return unsigned integer" $ do
          let evalOp = evalBinOp Sub (VInt 1      Unsigned) (VChar 'a'  )
          (evalOp, getType evalOp) `shouldBe` (VInt 4294967200 Unsigned, TInt Unsigned)
        it "should substract a bool and a bool and return signed integer" $ do
          let evalOp = evalBinOp Sub (VBool False ) (VBool True )
          (evalOp, getType evalOp) `shouldBe` (VInt (-1) Signed, TInt Signed)
        it "should substract a bool and a char and return signed integer" $ do
          let evalOp = evalBinOp Sub (VBool True ) (VChar 'a'  )
          (evalOp, getType evalOp) `shouldBe` (VInt (-96) Signed, TInt Signed)
        it "should substract a char and a char and return signed integer" $ do
          let evalOp = evalBinOp Sub (VChar 'a'  ) (VChar 'b'  )
          (evalOp, getType evalOp) `shouldBe` (VInt (-1) Signed, TInt Signed)
        it "should throw an error when subtracting anything with void" $ do
          evaluate (evalBinOp Sub VVoid (VInt 1 Signed)) `shouldThrow` errorCall (cannotEvalBinOpException Sub VVoid (VInt 1 Signed))
        it "should throw an error when subtracting anything with array" $ do
          evaluate (evalBinOp Sub (VArray 0 [] (TInt Signed)) (VInt 1 Signed)) `shouldThrow` errorCall (cannotEvalBinOpException Sub (VArray 0 [] (TInt Signed)) (VInt 1 Signed))

      context "Mul" $ do
        it "should multiply two signed integers" $ do
          let evalOp =  evalBinOp Mul (VInt 1      Signed  ) (VInt 2      Signed  )
          (evalOp, getType evalOp) `shouldBe` (VInt 2 Signed, TInt Signed)
        it "should multiply two unsigned integers" $ do
          let evalOp =  evalBinOp Mul (VInt 1      Unsigned) (VInt 2      Unsigned)
          (evalOp, getType evalOp) `shouldBe` (VInt 2 Unsigned, TInt Unsigned)
        it "should multiply a signed integer and an unsigned integer and return unsigned integer #1" $ do
          let evalOp =  evalBinOp Mul (VInt (-1)   Signed  ) (VInt 2      Unsigned)
          (evalOp, getType evalOp) `shouldBe` (VInt 4294967294 Unsigned, TInt Unsigned)
        it "should multiply a signed integer and an unsigned integer and return unsigned integer #2" $ do
          let evalOp =  evalBinOp Mul (VInt 1      Unsigned) (VInt (-2)   Signed  )
          (evalOp, getType evalOp) `shouldBe` (VInt 4294967294 Unsigned, TInt Unsigned)
        it "should multiply a signed integer and an unsigned integer and return unsigned integer #3" $ do
          let evalOp = evalBinOp Mul (VInt 76     Unsigned) (VInt (-103) Signed  )
          (evalOp, getType evalOp) `shouldBe` (VInt 4294959468 Unsigned, TInt Unsigned)
        it "should multiply a signed integer and an unsigned integer and return unsigned integer #4" $ do
          let evalOp = evalBinOp Mul (VInt (-76)  Signed  ) (VInt 103    Unsigned)
          (evalOp, getType evalOp) `shouldBe` (VInt 4294959468 Unsigned, TInt Unsigned)
        it "should multiply a signed integer and a bool and return signed integer" $ do
          let evalOp = evalBinOp Mul (VInt (-1)   Signed  ) (VBool True )
          (evalOp, getType evalOp) `shouldBe` (VInt (-1) Signed, TInt Signed)
        it "should multiply a signed integer and a char and return signed integer" $ do
          let evalOp = evalBinOp Mul (VInt 1   Signed  ) (VChar 'a'  )
          (evalOp, getType evalOp) `shouldBe` (VInt 97 Signed, TInt Signed)
        it "should multiply an unsigned integer and a bool and return unsigned integer" $ do
          let evalOp = evalBinOp Mul (VInt 1      Unsigned) (VBool True )
          (evalOp, getType evalOp) `shouldBe` (VInt 1 Unsigned, TInt Unsigned)
        it "should multiply an unsigned integer and a char and return unsigned integer" $ do
          let evalOp = evalBinOp Mul (VInt 1      Unsigned) (VChar 'a'  )
          (evalOp, getType evalOp) `shouldBe` (VInt 97 Unsigned, TInt Unsigned)
        it "should multiply a bool and a bool and return unsigned integer" $ do
          let evalOp = evalBinOp Mul (VBool False ) (VBool True )
          (evalOp, getType evalOp) `shouldBe` (VInt 0 Unsigned, TInt Unsigned)
        it "should multiply a bool and a char and return unsigned integer" $ do
          let evalOp = evalBinOp Mul (VBool True ) (VChar 'a'  )
          (evalOp, getType evalOp) `shouldBe` (VInt 97 Unsigned, TInt Unsigned)
        it "should multiply a char and a char and return unsigned integer" $ do
          let evalOp = evalBinOp Mul (VChar 'a'  ) (VChar 'b'  )
          (evalOp, getType evalOp) `shouldBe` (VInt 9506 Unsigned, TInt Unsigned)
        it "should throw an error when multiplying anything with void" $ do
          evaluate (evalBinOp Mul VVoid (VInt 1 Signed)) `shouldThrow` errorCall (cannotEvalBinOpException Mul VVoid (VInt 1 Signed))
        it "should throw an error when multiplying anything with array" $ do
          evaluate (evalBinOp Mul (VArray 0 [] (TInt Signed)) (VInt 1 Signed)) `shouldThrow` errorCall (cannotEvalBinOpException Mul (VArray 0 [] (TInt Signed)) (VInt 1 Signed))

      context "Div" $ do
        it "should divide two signed integers" $ do
          let evalOp =  evalBinOp Div (VInt (-4123)      Signed  ) (VInt 32      Signed  )
          (evalOp, getType evalOp) `shouldBe` (VInt (-128) Signed, TInt Signed)
        it "should divide two unsigned integers" $ do
          let evalOp =  evalBinOp Div (VInt 100      Unsigned) (VInt 2      Unsigned)
          (evalOp, getType evalOp) `shouldBe` (VInt 50 Unsigned, TInt Unsigned)
        it "should divide a signed integer and an unsigned integer and return unsigned integer #1" $ do
          let evalOp =  evalBinOp Div (VInt (-1)   Signed  ) (VInt 2      Unsigned)
          (evalOp, getType evalOp) `shouldBe` (VInt 2147483647 Unsigned, TInt Unsigned)
        it "should divide a signed integer and an unsigned integer and return unsigned integer #2" $ do
          let evalOp =  evalBinOp Div (VInt 1      Unsigned) (VInt (-2)   Signed  )
          (evalOp, getType evalOp) `shouldBe` (VInt 0 Unsigned, TInt Unsigned)
        it "should divide a signed integer and an unsigned integer and return unsigned integer #3" $ do
          let evalOp = evalBinOp Div (VInt 76     Unsigned) (VInt (-103) Signed  )
          (evalOp, getType evalOp) `shouldBe` (VInt 0 Unsigned, TInt Unsigned)
        it "should divide a signed integer and an unsigned integer and return unsigned integer #4" $ do
          let evalOp = evalBinOp Div (VInt (-76)  Signed  ) (VInt 103    Unsigned)
          (evalOp, getType evalOp) `shouldBe` (VInt 41698710 Unsigned, TInt Unsigned)
        it "should divide a signed integer and a bool and return signed integer" $ do
          let evalOp = evalBinOp Div (VInt (-1)   Signed  ) (VBool True )
          (evalOp, getType evalOp) `shouldBe` (VInt (-1) Signed, TInt Signed)
        it "should divide a signed integer and a char and return signed integer" $ do
          let evalOp = evalBinOp Div (VInt 3123   Signed  ) (VChar 'a'  )
          (evalOp, getType evalOp) `shouldBe` (VInt 32 Signed, TInt Signed)
        it "should divide an unsigned integer and a bool and return unsigned integer" $ do
          let evalOp = evalBinOp Div (VInt 1      Unsigned) (VBool True )
          (evalOp, getType evalOp) `shouldBe` (VInt 1 Unsigned, TInt Unsigned)
        it "should divide an unsigned integer and a char and return unsigned integer" $ do
          let evalOp = evalBinOp Div (VInt 1      Unsigned) (VChar 'a'  )
          (evalOp, getType evalOp) `shouldBe` (VInt 0 Unsigned, TInt Unsigned)
        it "should divide a bool and a bool and return unsigned integer" $ do
          let evalOp = evalBinOp Div (VBool False ) (VBool True )
          (evalOp, getType evalOp) `shouldBe` (VInt 0 Unsigned, TInt Unsigned)
        it "should divide a bool and a char and return unsigned integer" $ do
          let evalOp = evalBinOp Div (VBool True ) (VChar 'a'  )
          (evalOp, getType evalOp) `shouldBe` (VInt 0 Unsigned, TInt Unsigned)
        it "should divide a char and a char and return unsigned integer" $ do
          let evalOp = evalBinOp Div (VChar 'z'  ) (VChar 'a'  )
          (evalOp, getType evalOp) `shouldBe` (VInt 1 Unsigned, TInt Unsigned)
        it "should throw an error when dividing anything with void" $ do
          evaluate (evalBinOp Div VVoid (VInt 1 Signed)) `shouldThrow` errorCall (cannotEvalBinOpException Div VVoid (VInt 1 Signed))
        it "should throw an error when dividing anything with array" $ do
          evaluate (evalBinOp Div (VArray 0 [] (TInt Signed)) (VInt 1 Signed)) `shouldThrow` errorCall (cannotEvalBinOpException Div (VArray 0 [] (TInt Signed)) (VInt 1 Signed))

      context "Mod" $ do
        it "should modulo two signed integers" $ do
          let evalOp =  evalBinOp Mod (VInt (-4123)      Signed  ) (VInt 32      Signed  )
          (evalOp, getType evalOp) `shouldBe` (VInt (-27) Signed, TInt Signed)
        it "should modulo two unsigned integers" $ do
          let evalOp =  evalBinOp Mod (VInt 100      Unsigned) (VInt 2      Unsigned)
          (evalOp, getType evalOp) `shouldBe` (VInt 0 Unsigned, TInt Unsigned)
        it "should modulo a signed integer and an unsigned integer and return unsigned integer #1" $ do
          let evalOp =  evalBinOp Mod (VInt (-1)   Signed  ) (VInt 2      Unsigned)
          (evalOp, getType evalOp) `shouldBe` (VInt 1 Unsigned, TInt Unsigned)
        it "should modulo a signed integer and an unsigned integer and return unsigned integer #2" $ do
          let evalOp =  evalBinOp Mod (VInt 1      Unsigned) (VInt (-2)   Signed  )
          (evalOp, getType evalOp) `shouldBe` (VInt 1 Unsigned, TInt Unsigned)
        it "should modulo a signed integer and an unsigned integer and return unsigned integer #3" $ do
          let evalOp = evalBinOp Mod (VInt 76     Unsigned) (VInt (-103) Signed  )
          (evalOp, getType evalOp) `shouldBe` (VInt 76 Unsigned, TInt Unsigned)
        it "should modulo a signed integer and an unsigned integer and return unsigned integer #4" $ do
          let evalOp = evalBinOp Mod (VInt (-76)  Signed  ) (VInt 103    Unsigned)
          (evalOp, getType evalOp) `shouldBe` (VInt 90 Unsigned, TInt Unsigned)
        it "should modulo a signed integer and a bool and return signed integer" $ do
          let evalOp = evalBinOp Mod (VInt (-1)   Signed  ) (VBool True )
          (evalOp, getType evalOp) `shouldBe` (VInt 0 Signed, TInt Signed)
        it "should modulo a signed integer and a char and return signed integer" $ do
          let evalOp = evalBinOp Mod (VInt 3123   Signed  ) (VChar 'a'  )
          (evalOp, getType evalOp) `shouldBe` (VInt 19 Signed, TInt Signed)
        it "should modulo an unsigned integer and a bool and return unsigned integer" $ do
          let evalOp = evalBinOp Mod (VInt 1      Unsigned) (VBool True )
          (evalOp, getType evalOp) `shouldBe` (VInt 0 Unsigned, TInt Unsigned)
        it "should modulo an unsigned integer and a char and return unsigned integer" $ do
          let evalOp = evalBinOp Mod (VInt 1      Unsigned) (VChar 'a'  )
          (evalOp, getType evalOp) `shouldBe` (VInt 1 Unsigned, TInt Unsigned)
        it "should modulo a bool and a bool and return unsigned integer" $ do
          let evalOp = evalBinOp Mod (VBool False ) (VBool True )
          (evalOp, getType evalOp) `shouldBe` (VInt 0 Unsigned, TInt Unsigned)
        it "should modulo a bool and a char and return unsigned integer" $ do
          let evalOp = evalBinOp Mod (VBool True ) (VChar 'a'  )
          (evalOp, getType evalOp) `shouldBe` (VInt 1 Unsigned, TInt Unsigned)
        it "should modulo a char and a char and return unsigned integer" $ do
          let evalOp = evalBinOp Mod (VChar 'z'  ) (VChar 'a'  )
          (evalOp, getType evalOp) `shouldBe` (VInt 25 Unsigned, TInt Unsigned)
        it "should throw an error when moduloing anything with void" $ do
          evaluate (evalBinOp Mod VVoid (VInt 1 Signed)) `shouldThrow` errorCall (cannotEvalBinOpException Mod VVoid (VInt 1 Signed))
        it "should throw an error when moduloing anything with array" $ do
          evaluate (evalBinOp Mod (VArray 0 [] (TInt Signed)) (VInt 1 Signed)) `shouldThrow` errorCall (cannotEvalBinOpException Mod (VArray 0 [] (TInt Signed)) (VInt 1 Signed))

    context "Comparison" $ do
      context "Eq" $ do
        it "should compare two non-simular signed integers" $ do
          evalBinOp Eq (VInt (-4123)      Signed  ) (VInt 32      Signed  ) `shouldBe` VBool False
        it "should compare two simular signed integers" $ do
          evalBinOp Eq (VInt (-4123)      Signed  ) (VInt (-4123) Signed  ) `shouldBe` VBool True
        it "should compare two non-simular unsigned integers" $ do
          evalBinOp Eq (VInt 100      Unsigned) (VInt 2      Unsigned) `shouldBe` VBool False
        it "should compare two simular unsigned integers" $ do
          evalBinOp Eq (VInt 100      Unsigned) (VInt 100    Unsigned) `shouldBe` VBool True
        it "should compare non-simular signed and unsigned integers" $ do
          evalBinOp Eq (VInt (-1)   Signed  ) (VInt 2      Unsigned) `shouldBe` VBool False
        it "should compare simular signed and unsigned integers" $ do
          evalBinOp Eq (VInt 4294967295      Unsigned) (VInt (-1)   Signed  ) `shouldBe` VBool True
        it "should compare a non-simular signed integer and a bool" $ do
          evalBinOp Eq (VInt (-1)   Signed  ) (VBool False ) `shouldBe` VBool False
        it "should compare a simular signed integer and a bool" $ do
          evalBinOp Eq (VInt 1      Signed  ) (VBool True ) `shouldBe` VBool True
        it "should compare a non-simalar signed integer and a char" $ do
          evalBinOp Eq (VInt 3123   Signed  ) (VChar 'a'  ) `shouldBe` VBool False
        it "should compare a simular signed integer and a char" $ do
          evalBinOp Eq (VInt 97     Signed  ) (VChar 'a'  ) `shouldBe` VBool True
        it "should compare a non-simular unsigned integer and a bool" $ do
          evalBinOp Eq (VInt 1      Unsigned) (VBool False ) `shouldBe` VBool False
        it "should compare a simular unsigned integer and a bool" $ do
          evalBinOp Eq (VInt 1      Unsigned) (VBool True ) `shouldBe` VBool True
        it "should compare a non-simular unsigned integer and a char" $ do
          evalBinOp Eq (VInt 1      Unsigned) (VChar 'a'  ) `shouldBe` VBool False
        it "should compare a simular unsigned integer and a char" $ do
          evalBinOp Eq (VInt 97     Unsigned) (VChar 'a'  ) `shouldBe` VBool True
        it "should compare a non-simular bool and a bool" $ do
          evalBinOp Eq (VBool False ) (VBool True ) `shouldBe` VBool False
        it "should compare a simular bool and a bool" $ do
          evalBinOp Eq (VBool True ) (VBool True ) `shouldBe` VBool True
        it "should compare a non-simular bool and a char" $ do
          evalBinOp Eq (VBool False ) (VChar 'a'  ) `shouldBe` VBool False
        it "should compare a simular bool and a char" $ do
          evalBinOp Eq (VBool True ) (VChar '\1'  ) `shouldBe` VBool True
        it "should compare a non-simular char and a char" $ do
          evalBinOp Eq (VChar 'a'  ) (VChar 'b'  ) `shouldBe` VBool False
        it "should compare a simular char and a char" $ do
          evalBinOp Eq (VChar 'a'  ) (VChar 'a'  ) `shouldBe` VBool True
        it "should throw an error when comparing anything with void" $ do
          evaluate (evalBinOp Eq VVoid (VInt 1 Signed)) `shouldThrow` errorCall (cannotEvalBinOpException Eq VVoid (VInt 1 Signed))
        it "should throw an error when comparing anything with array" $ do
          evaluate (evalBinOp Eq (VArray 0 [] (TInt Signed)) (VInt 1 Signed)) `shouldThrow` errorCall (cannotEvalBinOpException Eq (VArray 0 [] (TInt Signed)) (VInt 1 Signed))

      context "Neq" $ do
        it "should compare two non-simular signed integers" $ do
          evalBinOp Neq (VInt (-4123)      Signed  ) (VInt 32      Signed  ) `shouldBe` VBool True
        it "should compare two simular signed integers" $ do
          evalBinOp Neq (VInt (-4123)      Signed  ) (VInt (-4123) Signed  ) `shouldBe` VBool False
        it "should compare two non-simular unsigned integers" $ do
          evalBinOp Neq (VInt 100      Unsigned) (VInt 2      Unsigned) `shouldBe` VBool True
        it "should compare two simular unsigned integers" $ do
          evalBinOp Neq (VInt 100      Unsigned) (VInt 100    Unsigned) `shouldBe` VBool False
        it "should compare non-simular signed and unsigned integers" $ do
          evalBinOp Neq (VInt (-1)   Signed  ) (VInt 2      Unsigned) `shouldBe` VBool True
        it "should compare simular signed and unsigned integers" $ do
          evalBinOp Neq (VInt 4294967295      Unsigned) (VInt (-1)   Signed  ) `shouldBe` VBool False
        it "should compare a non-simular signed integer and a bool" $ do
          evalBinOp Neq (VInt (-1)   Signed  ) (VBool False ) `shouldBe` VBool True
        it "should compare a simular signed integer and a bool" $ do
          evalBinOp Neq (VInt 1      Signed  ) (VBool True ) `shouldBe` VBool False
        it "should compare a non-simalar signed integer and a char" $ do
          evalBinOp Neq (VInt 3123   Signed  ) (VChar 'a'  ) `shouldBe` VBool True
        it "should compare a simular signed integer and a char" $ do
          evalBinOp Neq (VInt 97     Signed  ) (VChar 'a'  ) `shouldBe` VBool False
        it "should compare a non-simular unsigned integer and a bool" $ do
          evalBinOp Neq (VInt 1      Unsigned) (VBool False ) `shouldBe` VBool True
        it "should compare a simular unsigned integer and a bool" $ do
          evalBinOp Neq (VInt 1      Unsigned) (VBool True ) `shouldBe` VBool False
        it "should compare a non-simular unsigned integer and a char" $ do
          evalBinOp Neq (VInt 1      Unsigned) (VChar 'a'  ) `shouldBe` VBool True
        it "should compare a simular unsigned integer and a char" $ do
          evalBinOp Neq (VInt 97     Unsigned) (VChar 'a'  ) `shouldBe` VBool False
        it "should compare a non-simular bool and a bool" $ do
          evalBinOp Neq (VBool False ) (VBool True ) `shouldBe` VBool True
        it "should compare a simular bool and a bool" $ do
          evalBinOp Neq (VBool True ) (VBool True ) `shouldBe` VBool False
        it "should compare a non-simular bool and a char" $ do
          evalBinOp Neq (VBool False ) (VChar 'a'  ) `shouldBe` VBool True
        it "should compare a simular bool and a char" $ do
          evalBinOp Neq (VBool True ) (VChar '\1'  ) `shouldBe` VBool False
        it "should compare a non-simular char and a char" $ do
          evalBinOp Neq (VChar 'a'  ) (VChar 'b'  ) `shouldBe` VBool True
        it "should compare a simular char and a char" $ do
          evalBinOp Neq (VChar 'a'  ) (VChar 'a'  ) `shouldBe` VBool False
        it "should throw an error when comparing anything with void" $ do
          evaluate (evalBinOp Neq VVoid (VInt 1 Signed)) `shouldThrow` errorCall (cannotEvalBinOpException Neq VVoid (VInt 1 Signed))
        it "should throw an error when comparing anything with array" $ do
          evaluate (evalBinOp Neq (VArray 0 [] (TInt Signed)) (VInt 1 Signed)) `shouldThrow` errorCall (cannotEvalBinOpException Neq (VArray 0 [] (TInt Signed)) (VInt 1 Signed))

      context "Lt" $ do
        it "should return true when the first signed integer is smaller than the second" $ do
          evalBinOp Lt (VInt (-4123)      Signed  ) (VInt 32      Signed  ) `shouldBe` VBool True
        it "should return false when the first signed integer is bigger than the second" $ do
          evalBinOp Lt (VInt 32      Signed  ) (VInt (-4123)      Signed  ) `shouldBe` VBool False
        it "should return false when the first signed integer is equal to the second" $ do
          evalBinOp Lt (VInt (-4123)      Signed  ) (VInt (-4123) Signed  ) `shouldBe` VBool False
        it "should return true when the first unsigned integer is smaller than the second" $ do
          evalBinOp Lt (VInt 100      Unsigned) (VInt 2      Unsigned) `shouldBe` VBool False
        it "should return false when the first unsigned integer is bigger than the second" $ do
          evalBinOp Lt (VInt 2      Unsigned) (VInt 100      Unsigned) `shouldBe` VBool True
        it "should return false when the first unsigned integer is equal to the second" $ do
          evalBinOp Lt (VInt 100      Unsigned) (VInt 100    Unsigned) `shouldBe` VBool False
        it "should return true when the first signed integer is smaller than the second unsigned integer" $ do
          evalBinOp Lt (VInt (-100)   Signed  ) (VInt 4294967295      Unsigned) `shouldBe` VBool True
        it "should return false when the first signed integer is bigger than the second unsigned integer" $ do
          evalBinOp Lt (VInt (-1)   Signed  ) (VInt 2      Unsigned) `shouldBe` VBool False
        it "should return false when the first signed integer is equal to the second unsigned integer" $ do
          evalBinOp Lt (VInt (-1)   Signed  ) (VInt 4294967295      Unsigned) `shouldBe` VBool False
        

