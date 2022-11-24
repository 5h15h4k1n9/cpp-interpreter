module Errors where

import Ast
import TypeCheck

unexpectedTypeException :: CppType -> CppType -> String
unexpectedTypeException expected actual = 
  "Exception: Unexpected type: " ++ show actual ++ " (expected " ++ show expected ++ ")"
  
cannotConvertException :: CppValue -> CppType -> String
cannotConvertException from to = 
  "Exception: Cannot convert " ++ show (getType from) ++ " to " ++ show to
 
cannotEvalBinOpException :: BinOp -> CppValue -> CppValue -> String
cannotEvalBinOpException op v1 v2 = 
  "Exception: Cannot evaluate operation (" ++ show op ++ ") on " ++ show (getType v1) ++ " and " ++ show (getType v2)

cannotEvalUnOpException :: UnOp -> CppValue -> String
cannotEvalUnOpException op v = 
  "Exception: Cannot evaluate operation (" ++ show op ++ ") on " ++ show (getType v)

