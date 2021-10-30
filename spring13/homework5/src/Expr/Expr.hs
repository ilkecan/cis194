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
