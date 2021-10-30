-- Wholemeal programming
module Exercise1 where

fun1 :: [Integer] -> Integer
fun1 = product . map (subtract 2) . filter even

fun2 :: Integer -> Integer
fun2 = sum . filter even . takeWhile (> 0) . iterate collatz

collatz :: Integer -> Integer
collatz 1 = 0
collatz n
  | even n = n `div` 2
  | otherwise = 3 * n + 1
