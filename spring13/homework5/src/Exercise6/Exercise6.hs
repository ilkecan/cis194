{-# LANGUAGE FlexibleInstances #-}

module Exercise6 where

import Data.Map
  ( Map,
    fromList,
  )
import qualified Data.Map as M
  ( lookup,
  )
import Expr (Expr (add, lit, mul))

class HasVars a where
  var :: String -> a

data VarExprT
  = Lit !Integer
  | Add VarExprT VarExprT
  | Mul VarExprT VarExprT
  | Var !String
  deriving (Show, Eq)

instance Expr VarExprT where
  lit = Lit
  add = Add
  mul = Mul

instance HasVars VarExprT where
  var = Var

newtype VarExprT2 = VarExprT2 (Map String Integer -> Maybe Integer)

instance HasVars VarExprT2 where
  var = VarExprT2 . M.lookup

instance Expr VarExprT2 where
  lit = VarExprT2 . const . Just
  add = applyOperator (+)
  mul = applyOperator (*)

applyOperator ::
  (Integer -> Integer -> Integer) ->
  VarExprT2 ->
  VarExprT2 ->
  VarExprT2
applyOperator operator (VarExprT2 expr1) (VarExprT2 expr2) =
  VarExprT2 $ \vars -> case (expr1 vars, expr2 vars) of
    (Just n1, Just n2) -> Just $ operator n1 n2
    (_, _) -> Nothing

withVars :: [(String, Integer)] -> VarExprT2 -> Maybe Integer
withVars vars (VarExprT2 expr) = expr $ fromList vars
