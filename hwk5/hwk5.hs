{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

import qualified ExprT as E
import qualified StackVM
import qualified Data.Map as M
import Parser

eval :: E.ExprT -> Integer
eval (E.Lit n) = n
eval (E.Add x y) = eval x + eval y
eval (E.Mul x y) = eval x * eval y

evalStr str = parseExp E.Lit E.Add E.Mul str >>= return . eval

class Expr a where
    mul :: a -> a -> a
    add :: a -> a -> a
    lit :: Integer -> a

instance Expr E.ExprT where
    mul = E.Mul
    add = E.Add
    lit = E.Lit

instance Expr Integer where
    mul = (*)
    add = (+)
    lit = id

instance Expr Bool where
    mul = (&&)
    add = (||)
    lit = (>0)

newtype MinMax = MinMax Integer deriving (Eq, Show)
instance Expr MinMax where
    lit = MinMax
    mul (MinMax x) (MinMax y) = lit $ min x y
    add (MinMax x) (MinMax y) = lit $ max x y

newtype Mod7 = Mod7 Integer deriving (Eq, Show)
instance Expr Mod7 where
    lit = Mod7 . flip mod 7
    mul (Mod7 x) (Mod7 y) = lit $ x * y
    add (Mod7 x) (Mod7 y) = lit $ x + y

instance Expr StackVM.Program where
    lit x = [StackVM.PushI x]
    mul x y = x ++ y ++ [StackVM.Mul]  
    add x y = x ++ y ++ [StackVM.Add]

compile :: String -> Maybe StackVM.Program
compile = parseExp lit add mul

class HasVars a where
    var :: String -> a

data VarExprT = Lit Integer
           | Add VarExprT VarExprT
           | Mul VarExprT VarExprT
           | Var String
  deriving (Show, Eq)

instance HasVars VarExprT where
    var = Var

instance Expr VarExprT where
    mul = Mul
    add = Add
    lit = Lit

    
instance HasVars (M.Map String Integer -> Maybe Integer) where
    var = M.lookup

instance Expr (M.Map String Integer -> Maybe Integer) where
    lit = 
