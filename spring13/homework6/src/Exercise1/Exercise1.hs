module Exercise1 where

fib :: Integer -> Integer
fib 1 = 1
fib n = case compare n 0 of
  EQ -> 0
  LT ->
    if even n
      then - fib (- n)
      else fib (- n)
  GT -> fib (n - 1) + fib (n - 2)

fibs1 :: [Integer]
fibs1 = map fib [0 ..]
