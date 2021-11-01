module ExprT where

import Expr (Expr (add, lit, mul))

data ExprT
  = Lit !Integer
  | Add ExprT ExprT
  | Mul ExprT ExprT
  deriving (Show, Eq)

instance Expr ExprT where
  lit = Lit
  add = Add
  mul = Mul
