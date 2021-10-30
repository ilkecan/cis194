module Exercise2 where

import Exercise1 (eval)
import ExprT (ExprT (Add, Lit, Mul))
import Parser (parseExp)

evalStr :: String -> Maybe Integer
evalStr str = case parseExp Lit Add Mul str of
  (Just expr) -> Just $ eval expr
  Nothing -> Nothing
