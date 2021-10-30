module Expr where

import ExprT (ExprT (Add, Lit, Mul))

class Expr a where
  lit :: Integer -> a
  add :: a -> a -> a
  mul :: a -> a -> a

instance Expr ExprT where
  lit n = Lit n
  add expr1 expr2 = Add expr1 expr2
  mul expr1 expr2 = Mul expr1 expr2

instance Expr Integer where
  lit n = n
  add expr1 expr2 = expr1 + expr2
  mul expr1 expr2 = expr1 * expr2

instance Expr Bool where
  lit n
    | n > 0 = True
    | otherwise = False
  add expr1 expr2 = expr1 || expr2
  mul expr1 expr2 = expr1 && expr2

newtype MinMax = MinMax Integer
  deriving (Eq, Show)

instance Expr MinMax where
  lit n = MinMax n
  add (MinMax n1) (MinMax n2) = MinMax $ max n1 n2
  mul (MinMax n1) (MinMax n2) = MinMax $ min n1 n2

newtype Mod7 = Mod7 Integer
  deriving (Eq, Show)

instance Expr Mod7 where
  lit n = Mod7 $ n `mod` 7
  add (Mod7 n1) (Mod7 n2) = Mod7 $ (n1 + n2) `mod` 7
  mul (Mod7 n1) (Mod7 n2) = Mod7 $ (n1 * n2) `mod` 7
