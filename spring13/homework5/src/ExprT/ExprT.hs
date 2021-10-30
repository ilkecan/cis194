module ExprT where

import Expr (Expr (add, lit, mul))

data ExprT
  = Lit !Integer
  | Add ExprT ExprT
  | Mul ExprT ExprT
  deriving (Show, Eq)

instance Expr ExprT where
  lit n = Lit n
  add expr1 expr2 = Add expr1 expr2
  mul expr1 expr2 = Mul expr1 expr2
