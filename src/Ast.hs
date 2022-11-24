module Ast where

type Id = String
  
data IntegerTypeModifier = 
    Signed 
  | Unsigned
  deriving Eq
  
instance (Show IntegerTypeModifier) where
  show Signed   = "signed"
  show Unsigned = "unsigned"
  
data CppType = 
    TVoid                                   -- void foo();
  | TInt IntegerTypeModifier                -- signed int, unsigned int
  | TBool                                   -- bool
  | TChar                                   -- char
-- | TDouble                                 -- double
-- | TFun CppType [(Id, CppType)]            -- int bar(char x, bool y);
  | TArray CppType                          -- int arr[];
  deriving Eq

instance Show CppType where
  show TVoid      = "void"
  show (TInt m  ) = show m ++ "int"
  show TBool      = "bool"
  show TChar      = "char"
  show (TArray t) = show t ++ "[]"

data CppValue = 
    VVoid                                   -- void foo();
  | VInt Integer IntegerTypeModifier        -- 1, 2, 3
  | VBool Bool                              -- true, false
  | VChar Char                              -- 'a', 'b', 'c'
  | VArray Integer [CppValue] CppType       -- [1, 2, 3], "abc"

instance (Show CppValue) where
  show VVoid          = "void"
  show (VInt   v m  ) = show m ++ " " ++ show v
  show (VBool  v    ) = show v
  show (VChar  v    ) = show v
  show (VArray _ v t) = show t ++ " " ++ show v
  
data BinOp = 
  -- Arithmetic
    Add                                     -- +
  | Sub                                     -- -
  | Mul                                     -- *
  | Div                                     -- /
  | Mod                                     -- %
  -- Comparison
  | Eq                                      -- ==
  | Neq                                     -- !=
  | Lt                                      -- <
  | Gt                                      -- >
  | Leq                                     -- <=
  | Geq                                     -- >=
  -- Logical
  | And                                     -- &&
  | Or                                      -- ||
  | Xor                                     -- ^

instance (Show BinOp) where
  show Add = "+"
  show Sub = "-"
  show Mul = "*"
  show Div = "/"
  show Mod = "%"
  show Eq  = "=="
  show Neq = "!="
  show Lt  = "<"
  show Gt  = ">"
  show Leq = "<="
  show Geq = ">="
  show And = "&&"
  show Or  = "||"
  show Xor = "^"

data UnOp = 
    Neg                                     -- -
  | Not                                     -- !

instance (Show UnOp) where
  show Neg = "-"
  show Not = "!"
  
data CppExpr = 
    EVar Id                                 -- x
  | EVal CppValue                           -- 1, 'a', true
  | EBinOp BinOp CppExpr CppExpr            -- x + y
  | EUnOp UnOp CppExpr                      -- -x
  | EArray CppExpr CppExpr                  -- arr[0]

instance (Show CppExpr) where
  show (EVar   var     ) = var
  show (EVal   v       ) = show v
  show (EBinOp op e1 e2) = "(" ++ show e1 ++ " " ++ show op ++ " " ++ show e2 ++ ")"
  show (EUnOp  op e    ) = "(" ++ show op ++ show e ++ ")"
  show (EArray e1 e2   ) = show e1 ++ "[" ++ show e2 ++ "]"
  
data CppStmt = 
    SVar Id CppType CppExpr                 -- int x = 1;
  | SAssign CppExpr CppExpr                 -- x = 1;
  | SConst Id CppType CppExpr               -- const char x = 'a';
-- | SPrintf CppExpr [CppExpr]               -- printf("%d", x);
  | SBlock [CppStmt]                        -- { x = 1; y = 2; }
  | SIf CppExpr CppStmt                     -- if (x) { y = 1; }
  | SIfElse CppExpr CppStmt CppStmt         -- if (x) { y = 1; } else { y = 2; }
  | SWhile CppExpr CppStmt                  -- while (x) { y = 1; }
  | SFor CppExpr CppExpr CppExpr CppStmt    -- for (int i = 0; i < 10; i++) { y = 1; }
  | SReturn CppExpr                         -- return x;
  deriving Show
  
{-
data CppFunc = 
    CppFunc Id CppType [(Id, CppType)] [CppStmt] -- int foo(char x, bool y) { x = 1; y = 2; }
  deriving (Eq, Show)
-}

newtype CppProgram = CppProgram [CppStmt]   -- int main() { int x = 1; char y = 'a'; if (x > 0) { y = 'b'; } return 0; else { y = 'c'; } return 1; }
  deriving Show
  
  
