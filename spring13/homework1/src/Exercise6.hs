module Exercise6 where

import Data.Ratio
import Exercise5
  ( Move,
    Peg,
    hanoi,
  )

hanoi4 :: Integer -> Peg -> Peg -> Peg -> Peg -> [Move]
hanoi4 0 _ _ _ _ = []
hanoi4 1 p1 p2 _ _ = [(p1, p2)]
hanoi4 n p1 p2 p3 p4 = hanoi4 nMinusK p1 p4 p2 p3 ++ hanoi k p1 p2 p3 ++ hanoi4 nMinusK p4 p2 p1 p3
  where
    k = idealK n
    nMinusK = n - k

-- https://berkeycolortran.wordpress.com/2014/04/05/week-12-update/
idealK1 :: Integer -> Integer
idealK1 n = floor (sqrt (2 * (fromInteger n :: Double) + 9 / 4) - 1 / 2)

idealK2 :: Integer -> Integer
idealK2 n = floor (sqrt (fromRational (2 * fromInteger n + 9 % 4) :: Double) - 1 / 2)

idealK :: Integer -> Integer
idealK = idealK1
