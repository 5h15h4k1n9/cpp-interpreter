module ValueOperations where

import Ast
import Errors

toSInt :: CppValue -> CppValue
toSInt v@VVoid       = error $ cannotConvertException v (TInt Signed)
toSInt (VInt    i _) = VInt ni                       Signed where
    maxSInt = 2147483648
    iDiv = i `div` maxSInt
    iMod = i `mod` maxSInt
    ni = iMod - (iDiv `mod` 2) * maxSInt
toSInt (VBool   b  ) = VInt (if b then 1 else 0)     Signed
toSInt (VChar   c  ) = VInt (toInteger $ fromEnum c) Signed
toSInt v@VArray {}   = error $ cannotConvertException v (TInt Signed)

toUInt :: CppValue -> CppValue
toUInt v@VVoid       = error $ cannotConvertException v (TInt Unsigned)
toUInt (VInt    i _) = VInt (i `mod` 4294967296)     Unsigned
toUInt (VBool   b  ) = VInt (if b then 1 else 0)     Unsigned
toUInt (VChar   c  ) = VInt (toInteger $ fromEnum c) Unsigned
toUInt v@VArray {}   = error $ cannotConvertException v (TInt Unsigned)

toBool :: CppValue -> CppValue
toBool v@VVoid       = error $ cannotConvertException v TBool
toBool (VInt    i _) = VBool (i /= 0)
toBool (VBool   b  ) = VBool b
toBool (VChar   c  ) = VBool (c /= '\0')
toBool v@VArray {}   = error $ cannotConvertException v TBool

instance (Num CppValue) where
  -- (+)
  (VInt i1 Signed)     + (VInt i2 m) = case m of
    Signed                          -> toSInt $ VInt (i1 + i2) Signed
    Unsigned                        -> toUInt $ VInt (i1 + i2) Unsigned
  (VInt i1 Unsigned)   + (VInt i2 _) = toUInt $ VInt (i1 + i2) Unsigned
  v1@(VInt _ Signed)   + v2          = v1 + toSInt v2
  v1@(VInt _ Unsigned) + v2          = v1 + toUInt v2
  v1                   + v2          = toUInt v1 + toUInt v2
  -- (*)
  (VInt i1 Signed)     * (VInt i2 m) = case m of
    Signed                          -> toSInt $ VInt (i1 * i2) Signed
    Unsigned                        -> toUInt $ VInt (i1 * i2) Unsigned
  (VInt i1 Unsigned)   * (VInt i2 _) = toUInt $ VInt (i1 * i2) Unsigned
  v1@(VInt _ Signed)   * v2          = v1 * toSInt v2
  v1@(VInt _ Unsigned) * v2          = v1 * toUInt v2
  v1                   * v2          = toUInt v1 * toUInt v2
  -- (-)
  (VInt i1 Signed)     - (VInt i2 m) = case m of
    Signed                          -> toSInt $ VInt (i1 - i2) Signed
    Unsigned                        -> toUInt $ VInt (i1 - i2) Unsigned
  (VInt i1 Unsigned)   - (VInt i2 _) = toUInt $ VInt (i1 - i2) Unsigned
  v1@(VInt _ Signed)   - v2          = v1 - toSInt v2
  v1@(VInt _ Unsigned) - v2          = v1 - toUInt v2
  v1                   - v2          = toUInt v1 - toUInt v2
  -- abs
  abs (VInt i m)                     = VInt (abs i) m
  abs v                              = toUInt $ abs $ toUInt v
  -- signum
  signum (VInt i m)                  = VInt (signum i) m
  signum v                           = toUInt $ signum $ toUInt v
  -- fromInteger
  fromInteger i                      = VInt i Unsigned
  -- negate
  negate (VInt i _)                  = VInt (negate i) Signed
  negate v                           = toSInt $ negate $ toSInt v
  
instance (Fractional CppValue) where
  -- (/)
  (VInt i1 Signed)     / (VInt i2 m) = case m of
    Signed                          -> toSInt $ VInt (i1 `div` i2) Signed
    Unsigned                        -> toUInt $ VInt (i1 `div` i2) Unsigned
  (VInt i1 Unsigned)   / (VInt i2 _) = toUInt $ VInt (i1 `div` i2) Unsigned
  v1@(VInt _ Signed)   / v2          = v1 / toSInt v2
  v1@(VInt _ Unsigned) / v2          = v1 / toUInt v2
  v1                   / v2          = toUInt v1 / toUInt v2 
  
instance (Eq CppValue) where
  (VInt  i1 _) == (VInt i2 _) = i1 == i2
  (VBool b1  ) == (VBool b2)  = b1 == b2
  (VChar c1  ) == (VChar c2)  = c1 == c2
  v1           == v2          = toUInt v1 == toUInt v2
  
instance (Ord CppValue) where
  compare (VInt  i1 _) (VInt  i2 _) = compare i1 i2
  compare (VBool b1  ) (VBool b2  ) = compare b1 b2
  compare (VChar c1  ) (VChar c2  ) = compare c1 c2
  compare v1           v2           = compare (toUInt v1) (toUInt v2) 
