module TypeCheck(module TypeCheck) where

import Ast

getType :: CppValue -> CppType
getType  VVoid                  = TVoid
getType (VInt   _     modifier) = 
  case modifier of
    Signed                     -> TInt Signed
    Unsigned                   -> TInt Unsigned
getType (VBool  _             ) = TBool
getType (VChar  _             ) = TChar
getType (VArray elems t       ) = TArray t (length elems)

checkType :: CppValue -> CppType -> Bool
checkType v t = getType v == t

compareTypes :: CppValue -> CppValue -> Bool
compareTypes v1 v2 = getType v1 == getType v2
