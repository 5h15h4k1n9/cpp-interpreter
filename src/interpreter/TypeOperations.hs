module TypeOperations where

import Ast
import Errors

toSInt :: CppValue -> CppValue
toSInt VVoid          = error $ cannotConvertException TVoid (TInt Signed)
toSInt (VInt   v _  ) = VInt nv                       Signed where
    maxSInt = 2147483648
    vDiv = v `div` maxSInt
    vMod = v `mod` maxSInt
    nv = vMod - (vDiv `mod` 2) * maxSInt
toSInt (VBool  v    ) = VInt (if v then 1 else 0)     Signed
toSInt (VChar  v    ) = VInt (toInteger $ fromEnum v) Signed
toSInt (VArray _ _ t) = error $ cannotConvertException (TArray t) (TInt Signed)

toUInt :: CppValue -> CppValue
toUInt VVoid          = error $ cannotConvertException TVoid (TInt Unsigned)
toUInt (VInt   v _  ) = VInt (v `mod` 4294967296)     Unsigned
toUInt (VBool  v    ) = VInt (if v then 1 else 0)     Unsigned
toUInt (VChar  v    ) = VInt (toInteger $ fromEnum v) Unsigned
toUInt (VArray _ _ t) = error $ cannotConvertException (TArray t) (TInt Unsigned)

toBool :: CppValue -> CppValue
toBool VVoid          = error $ cannotConvertException TVoid TBool
toBool (VInt   v _  ) = VBool (v /= 0)
toBool (VBool  v    ) = VBool v
toBool (VChar  v    ) = VBool (v /= '\0')
toBool (VArray _ _ t) = error $ cannotConvertException (TArray t) TBool