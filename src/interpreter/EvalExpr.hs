module EvalExpr(module EvalExpr) where

import Ast
import ValueOperations
import Errors

evalBinOp :: BinOp -> CppValue -> CppValue -> CppValue
evalBinOp op  v1@VArray {} v2           = error $ cannotEvalBinOpException op v1 v2
evalBinOp op  v1           v2@VArray {} = error $ cannotEvalBinOpException op v1 v2
evalBinOp op  v1@VVoid     v2           = error $ cannotEvalBinOpException op v1 v2
evalBinOp op  v1           v2@VVoid     = error $ cannotEvalBinOpException op v1 v2
-- Arithmetic
evalBinOp Add v1 v2 = v1   +   v2
evalBinOp Sub v1 v2 = v1   -   v2
evalBinOp Mul v1 v2 = v1   *   v2
evalBinOp Div v1 v2 = v1 `quot` v2
evalBinOp Mod v1 v2 = v1 `rem` v2
-- Comparison
evalBinOp Eq  v1 v2 = VBool (v1 == v2)
evalBinOp Neq v1 v2 = VBool (v1 /= v2)
evalBinOp Lt  v1 v2 = VBool (v1 <  v2)
evalBinOp Gt  v1 v2 = VBool (v1 >  v2)
evalBinOp Leq v1 v2 = VBool (v1 <= v2)
evalBinOp Geq v1 v2 = VBool (v1 >= v2)
-- Logical
evalBinOp And v1 v2 = v1 `cppAnd` v2
evalBinOp Or  v1 v2 = v1 `cppOr` v2

evalUnOp :: UnOp -> CppValue -> CppValue
evalUnOp op  v@VArray {} = error $ cannotEvalUnOpException op v
evalUnOp op  v@VVoid     = error $ cannotEvalUnOpException op v
evalUnOp Neg v           = -v
evalUnOp Not (VBool b  ) = VBool (not b)
evalUnOp Not v           = evalUnOp Not (toBool v)
