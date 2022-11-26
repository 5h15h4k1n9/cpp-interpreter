module ValueOperationsSpec(spec) where

import Ast
import ValueOperations
import Errors

import Test.Hspec.Expectations (shouldBe, shouldThrow, errorCall)
import Test.Hspec.Core.Spec (describe, it, context, Spec)
import Control.Exception.Base (evaluate)

spec :: Spec
spec = do
  describe "TypeOperations" $ do
    describe "toSInt" $ do
      context "VVoid" $ do
        it "should throw a cannot convert exception" $ do
          evaluate (toSInt VVoid) `shouldThrow` errorCall (cannotConvertException VVoid (TInt Signed))
      context "VInt" $ do
        it "should convert a signed integer to a signed integer" $ do
          toSInt (VInt (-1)          Signed  ) `shouldBe` VInt (-1)          Signed
        it "should convert an unsigned integer to a signed integer" $ do
          toSInt (VInt 1             Unsigned) `shouldBe` VInt 1             Signed
        it "should convert overflow signed integer to a signed integer #1" $ do
          toSInt (VInt 2147483648    Signed  ) `shouldBe` VInt (-2147483648) Signed
        it "should convert overflow signed integer to a signed integer #2" $ do
          toSInt (VInt (-2147483649) Signed  ) `shouldBe` VInt 2147483647    Signed
        it "should convert overflow unsigned integer to a signed integer" $ do
          toSInt (VInt 4294967296    Unsigned) `shouldBe` VInt 0             Signed
      context "VBool" $ do
        it "should convert a bool true to a signed integer" $ do
          toSInt (VBool True ) `shouldBe` VInt 1 Signed
        it "should convert a bool false to a signed integer" $ do
          toSInt (VBool False) `shouldBe` VInt 0 Signed
      context "VChar" $ do
        it "should convert a char to a signed integer" $ do
          toSInt (VChar 'a') `shouldBe` VInt 97 Signed
      context "VArray" $ do
        it "should throw a cannot convert exception" $ do
          evaluate (toSInt (VArray 0 [] TChar)) `shouldThrow` errorCall (cannotConvertException (VArray 0 [] TChar) (TInt Signed))
    
    describe "toUInt" $ do
      context "VVoid" $ do
        it "should throw a cannot convert exception" $ do
          evaluate (toUInt VVoid) `shouldThrow` errorCall (cannotConvertException VVoid (TInt Unsigned))
      context "VInt" $ do
        it "should convert a signed integer to an unsigned integer" $ do
          toUInt (VInt (-1)       Signed  ) `shouldBe` VInt 4294967295 Unsigned
        it "should convert an unsigned integer to an unsigned integer" $ do
          toUInt (VInt 1          Unsigned) `shouldBe` VInt 1          Unsigned
        it "should convert overflow unsigned integer to an unsigned integer" $ do
          toUInt (VInt 4294967296 Unsigned) `shouldBe` VInt 0          Unsigned
      context "VBool" $ do
        it "should convert a bool true to an unsigned integer" $ do
          toUInt (VBool True ) `shouldBe` VInt 1 Unsigned
        it "should convert a bool false to an unsigned integer" $ do
          toUInt (VBool False) `shouldBe` VInt 0 Unsigned
      context "VChar" $ do
        it "should convert a char to an unsigned integer" $ do
          toUInt (VChar 'a') `shouldBe` VInt 97 Unsigned
      context "VArray" $ do
        it "should throw a cannot convert exception" $ do
          evaluate (toUInt (VArray 0 [] TChar)) `shouldThrow` errorCall (cannotConvertException (VArray 0 [] TChar) (TInt Unsigned))

    describe "toBool" $ do
      context "VVoid" $ do
        it "should throw a cannot convert exception" $ do
          evaluate (toBool VVoid) `shouldThrow` errorCall (cannotConvertException VVoid TBool)
      context "VInt" $ do
        it "should convert a signed integer to a bool true" $ do
          toBool (VInt (-1) Signed  ) `shouldBe` VBool True
        it "should convert an unsigned integer to a bool true" $ do
          toBool (VInt 1    Unsigned) `shouldBe` VBool True
        it "should convert a signed integer to a bool false" $ do
          toBool (VInt 0    Signed  ) `shouldBe` VBool False
        it "should convert an unsigned integer to a bool false" $ do
          toBool (VInt 0    Unsigned) `shouldBe` VBool False
      context "VBool" $ do
        it "should convert a bool to a bool" $ do
          toBool (VBool True) `shouldBe` VBool True
      context "VChar" $ do
        it "should convert a char to a bool true" $ do
          toBool (VChar 'a' ) `shouldBe` VBool True
        it "should convert a char to a bool false" $ do
          toBool (VChar '\0') `shouldBe` VBool False
      context "VArray" $ do
        it "should throw a cannot convert exception" $ do
          evaluate (toBool (VArray 0 [] TBool)) `shouldThrow` errorCall (cannotConvertException (VArray 0 [] TBool) TBool)
