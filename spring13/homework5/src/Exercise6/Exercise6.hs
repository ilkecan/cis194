{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

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

type VarExprT2 = (Map String Integer -> Maybe Integer)

instance HasVars VarExprT2 where
  var = M.lookup

instance Expr VarExprT2 where
  lit = const . Just
  add = applyOperator (+)
  mul = applyOperator (*)

applyOperator ::
  (Integer -> Integer -> Integer) ->
  VarExprT2 ->
  VarExprT2 ->
  Map String Integer ->
  Maybe Integer
applyOperator operator expr1 expr2 vars = case (expr1 vars, expr2 vars) of
  (Just n1, Just n2) -> Just $ operator n1 n2
  (_, _) -> Nothing

withVars :: [(String, Integer)] -> VarExprT2 -> Maybe Integer
withVars vars expr = expr $ fromList vars
