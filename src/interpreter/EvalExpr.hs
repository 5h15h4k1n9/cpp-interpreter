module EvalExpr(module EvalExpr) where

import Ast
import TypeOperations

evalBinOp :: BinOp -> CppValue -> CppValue -> CppValue
evalBinOp Add v1@(VInt x Signed) v2 = case v2 of
  VInt y Signed   -> VInt (x + y) Signed
  VInt y Unsigned -> VInt (x + y) Signed
  _               -> evalBinOp Add v1 (toInt v2)
evalBinOp _   _                  _  = undefined