module ValueOperations where

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

instance (Num CppValue) where
  -- (+)
  (VInt v1 Signed)     + (VInt v2 m) = case m of
    Signed                          -> toSInt $ VInt (v1 + v2) Signed
    Unsigned                        -> toUInt $ VInt (v1 + v2) Unsigned
  (VInt v1 Unsigned)   + (VInt v2 _) = toUInt $ VInt (v1 + v2) Unsigned
  v1@(VInt _ Signed)   + v2          = v1 + toSInt v2
  v1@(VInt _ Unsigned) + v2          = v1 + toUInt v2
  v1                   + v2          = toUInt v1 + toUInt v2
  -- (*)
  (VInt v1 Signed)     * (VInt v2 m) = case m of
    Signed                          -> toSInt $ VInt (v1 * v2) Signed
    Unsigned                        -> toUInt $ VInt (v1 * v2) Unsigned
  (VInt v1 Unsigned)   * (VInt v2 _) = toUInt $ VInt (v1 * v2) Unsigned
  v1@(VInt _ Signed)   * v2          = v1 * toSInt v2
  v1@(VInt _ Unsigned) * v2          = v1 * toUInt v2
  v1                   * v2          = toUInt v1 * toUInt v2
  -- (-)
  (VInt v1 Signed)     - (VInt v2 m) = case m of
    Signed                          -> toSInt $ VInt (v1 - v2) Signed
    Unsigned                        -> toUInt $ VInt (v1 - v2) Unsigned
  (VInt v1 Unsigned)   - (VInt v2 _) = toUInt $ VInt (v1 - v2) Unsigned
  v1@(VInt _ Signed)   - v2          = v1 - toSInt v2
  v1@(VInt _ Unsigned) - v2          = v1 - toUInt v2
  v1                   - v2          = toUInt v1 - toUInt v2
  -- abs
  abs (VInt v m)                    = VInt (abs v) m
  abs v                             = toUInt $ abs $ toUInt v
  -- signum
  signum (VInt v m)                 = VInt (signum v) m
  signum v                          = toUInt $ signum $ toUInt v
  -- fromInteger
  fromInteger i                     = VInt i Unsigned
  -- negate
  negate (VInt v _)                 = VInt (negate v) Signed
  negate v                          = toSInt $ negate $ toSInt v
  
instance (Eq CppValue) where
  (VInt v1 m1) == (VInt v2 m2)     = v1 == v2
  (VBool v1) == (VBool v2)         = v1 == v2
  (VChar v1) == (VChar v2)         = v1 == v2
  v1         == v2                 = toUInt v1 == toUInt v2
  
instance (Ord CppValue) where
  compare (VInt v1 m1) (VInt v2 m2) = compare v1 v2
  compare (VBool v1) (VBool v2)     = compare v1 v2
  compare (VChar v1) (VChar v2)     = compare v1 v2
  compare v1 v2                     = compare (toUInt v1) (toUInt v2)