-- Fibonacci numbers via matrices
module Exercise7 where

import Data.List (transpose)

newtype Matrix a = Matrix {fromMatrix :: [[a]]}
  deriving (Show, Eq)

instance Num a => Num (Matrix a) where
  (+) (Matrix m1) (Matrix m2) = Matrix $ zipWith (zipWith (+)) m1 m2
  (*) (Matrix m1) (Matrix m2) = Matrix $ map f m1
    where
      f row = map (sum . zipWith (*) row) (transpose m2)
  abs = applyFunc abs
  signum = applyFunc signum
  fromInteger n = Matrix [[n', 0], [0, n']]
    where
      n' = fromInteger n
  negate = applyFunc negate

applyFunc ::
  (a -> a) ->
  Matrix a ->
  Matrix a
applyFunc f (Matrix m) = Matrix $ map (map f) m

fibonacciQMatrix :: Matrix Integer
fibonacciQMatrix = Matrix [[1, 1], [1, 0]]

fib4 :: Integer -> Integer
fib4 0 = 0
fib4 n = (head . fromMatrix) (fibonacciQMatrix ^ n) !! 1
