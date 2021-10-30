module Exercise5 where

import Expr (Expr (add, lit, mul))
import Parser (parseExp)
import StackVM (Program)

compile :: String -> Maybe Program
compile = parseExp lit add mul
