module TypeOperations(module TypeOperations) where

import Ast
import Errors

toInt :: CppValue -> CppValue
toInt  VVoid       = error $ cannotConvertException TVoid (TInt Signed)
toInt (VInt   _ _) = undefined
toInt (VBool  v  ) = VInt (if v then 1 else 0)     Signed
toInt (VChar  v  ) = VInt (toInteger $ fromEnum v) Signed
toInt (VArray _ t) = error $ cannotConvertException (TArray t 0) (TInt Signed)

toBool :: CppValue -> CppValue
toBool  VVoid       = error $ cannotConvertException TVoid TBool
toBool (VInt   v _) = VBool (v /= 0)
toBool (VBool  v  ) = VBool v
toBool (VChar  v  ) = VBool (v /= '\0')
toBool (VArray _ _) = error $ cannotConvertException (TArray TVoid 0) TBool