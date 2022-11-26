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
  v1@(VInt i1 Signed  ) + v2@(VInt i2 m) = case m of
    Signed                              -> toSInt $ VInt (i1 + i2) Signed
    Unsigned                            -> toUInt v1 + v2
  v1@(VInt i1 Unsigned) + v2@(VInt i2  m) = case m of
    Signed                              -> v1 + toUInt v2
    Unsigned                            -> toUInt $ VInt (i1 + i2) Unsigned
  v1@(VInt _  Signed  ) + v2             = v1 + toSInt v2
  v1@(VInt _  Unsigned) + v2             = v1 + toUInt v2
  v1                    + v2             = toUInt v1 + toUInt v2
  -- (*)
  v1@(VInt i1 Signed  ) * v2@(VInt i2 m) = case m of
    Signed                              -> toSInt $ VInt (i1 * i2) Signed
    Unsigned                            -> toUInt v1 * v2
  v1@(VInt i1 Unsigned) * v2@(VInt i2 m) = case m of
    Signed                              -> v1 * toUInt v2
    Unsigned                            -> toUInt $ VInt (i1 * i2) Unsigned
  v1@(VInt _  Signed  ) * v2             = v1 * toSInt v2
  v1@(VInt _  Unsigned) * v2             = v1 * toUInt v2
  v1                    * v2             = toUInt v1 * toUInt v2
  -- (-)
  v1@(VInt i1 Signed  ) - v2@(VInt i2 m) = case m of
    Signed                              -> toSInt $ VInt (i1 - i2) Signed
    Unsigned                            -> toUInt v1 - v2
  v1@(VInt i1 Unsigned) - v2@(VInt i2  m) = case m of
    Signed                              -> v1 - toUInt v2
    Unsigned                            -> toUInt $ VInt (i1 - i2) Unsigned
  v1@(VInt _ Signed   ) - v2             = v1 - toSInt v2
  v1@(VInt _ Unsigned ) - v2             = v1 - toUInt v2
  v1                    - v2             = toSInt v1 - toSInt v2
  -- abs
  abs (VInt i m) = VInt (abs i) m
  abs v          = toUInt $ abs $ toUInt v
  -- signum
  signum (VInt i m) = VInt (signum i) m
  signum v          = toUInt $ signum $ toUInt v
  -- fromInteger
  fromInteger i = VInt i Unsigned
  -- negate 
  negate (VInt i _) = VInt (negate i) Signed
  negate v          = toSInt $ negate $ toSInt v

instance (Enum CppValue) where
  -- toEnum
  toEnum i = toUInt $ VInt (toInteger i) Unsigned
  -- fromEnum
  fromEnum (VInt i Unsigned) = fromInteger i
  fromEnum v                 = fromEnum $ toUInt v
  
instance (Real CppValue) where
  -- toRational
  toRational (VInt i Unsigned) = toRational i
  toRational v                 = toRational $ toUInt v

instance (Integral CppValue) where
  -- quotRem
  quotRem v1@(VInt i1 Signed  ) v2@(VInt i2 m) = case m of
    Signed                        -> (toSInt $ VInt (i1 `quot` i2) Signed,
                                      toSInt $ VInt (i1 `rem`  i2) Signed)
    Unsigned                      -> quotRem (toUInt v1) v2
  quotRem v1@(VInt i1 Unsigned) v2@(VInt i2 m) = case m of
    Signed                        -> quotRem v1 (toUInt v2)
    Unsigned                      -> (toUInt $ VInt (i1 `quot` i2) Unsigned,
                                      toUInt $ VInt (i1 `rem`  i2) Unsigned)
  quotRem v1@(VInt _  Signed  ) v2 = quotRem v1 (toSInt v2)
  quotRem v1@(VInt _  Unsigned) v2 = quotRem v1 (toUInt v2)
  quotRem v1                    v2 = quotRem (toUInt v1) (toUInt v2)
  -- toInteger
  toInteger (VInt i Unsigned) = i
  toInteger v                 = toInteger $ toUInt v
 
instance (Eq CppValue) where
  (VInt  i1 Signed  ) == (VInt  i2 Signed  ) = i1 == i2
  (VInt  i1 Unsigned) == (VInt  i2 Unsigned) = i1 == i2
  (VBool b1         ) == (VBool b2         ) = b1 == b2
  (VChar c1         ) == (VChar c2         ) = c1 == c2
  v1                  == v2                  = toUInt v1 == toUInt v2
  
instance (Ord CppValue) where
  compare (VInt  i1 Signed  ) (VInt  i2 Signed  ) = compare i1 i2
  compare (VInt  i1 Unsigned) (VInt  i2 Unsigned) = compare i1 i2
  compare (VBool b1         ) (VBool b2         ) = compare b1 b2
  compare (VChar c1         ) (VChar c2         ) = compare c1 c2
  compare v1                  v2                  = compare (toUInt v1) (toUInt v2) 

cppAnd :: CppValue -> CppValue -> CppValue
cppAnd (VBool b1) (VBool b2) = VBool (b1 && b2)
cppAnd v1        v2          = toBool v1 `cppAnd` toBool v2

cppOr :: CppValue -> CppValue -> CppValue
cppOr (VBool b1) (VBool b2) = VBool (b1 || b2)
cppOr v1        v2          = toBool v1 `cppOr` toBool v2
