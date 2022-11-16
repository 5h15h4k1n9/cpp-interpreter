module Errors(module Errors) where

import Ast

showType :: CppType -> String
showType  TVoid       = "void"
showType (TInt   m  ) = sign ++ "int" where
  sign = case m of
    Signed   -> "signed "
    Unsigned -> "unsigned "
showType  TBool       = "bool"
showType  TChar       = "char"
showType (TArray t _) = showType t ++ "[]"

unexpectedTypeException :: CppType -> CppType -> String
unexpectedTypeException expected actual = 
  "Exception: Unexpected type: " ++ showType actual ++ " (expected " ++ showType expected ++ ")"
  
cannotConvertException :: CppType -> CppType -> String
cannotConvertException from to = 
  "Exception: Cannot convert " ++ showType from ++ " to " ++ showType to

