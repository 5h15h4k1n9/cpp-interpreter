module Ast(module Ast) where

type Id = String
  
data IntegerTypeModifier = 
    Signed 
  | Unsigned
  deriving (Eq, Show)
  
data CppType = 
    TVoid                                   -- void foo();
  | TInt IntegerTypeModifier                -- signed int, unsigned int
  | TBool                                   -- bool
  | TChar                                   -- char
-- | TDouble                                 -- double
-- | TFun CppType [(Id, CppType)]            -- int bar(char x, bool y);
  | TArray CppType Int                      -- int arr[10];
  deriving (Eq, Show)
  
data CppValue = 
    VVoid                                   -- void foo();
  | VInt Integer IntegerTypeModifier        -- 1, 2, 3
  | VBool Bool                              -- true, false
  | VChar Char                              -- 'a', 'b', 'c'
  | VArray [CppValue] CppType               -- [1, 2, 3], "abc"
  deriving (Eq, Show)
  
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
  deriving (Eq, Show)

data UnOp = 
    Neg                                     -- -
  | Not                                     -- !
  deriving (Eq, Show)
  
data CppExpr = 
    EVar Id                                 -- x
  | EVal CppValue                           -- 1, 'a', true
  | EBinOp BinOp CppExpr CppExpr            -- x + y
  | EUnOp UnOp CppExpr                      -- -x
  | EArray CppExpr CppExpr                  -- arr[0]
  deriving (Eq, Show)
  
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
  deriving (Eq, Show)
  
{-
data CppFunc = 
    CppFunc Id CppType [(Id, CppType)] [CppStmt] -- int foo(char x, bool y) { x = 1; y = 2; }
  deriving (Eq, Show)
-}

newtype CppProgram = CppProgram [CppStmt]   -- int main() { int x = 1; char y = 'a'; if (x > 0) { y = 'b'; } return 0; else { y = 'c'; } return 1; }
  deriving (Eq, Show)
  
  
