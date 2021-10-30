-- Finding primes
module Exercise4 where

import Data.List (sort)
import Data.List.Ordered
  ( minus,
    union,
  )

sieveSundaram1 :: Integer -> [Integer]
sieveSundaram1 n = map (\x -> 2 * x + 1) . minus [1 .. n] . sundaram1 $ n

sieveSundaram2 :: Integer -> [Integer]
sieveSundaram2 n = map (\x -> 2 * x + 1) . minus [1 .. n] . sundaram2 $ n

sieveSundaram :: Integer -> [Integer]
sieveSundaram = sieveSundaram1

sundaram1 :: Integer -> [Integer]
sundaram1 n =
  sort
    [ i + j + 2 * i * j
      | let n' = fromInteger n :: Double,
        i <- [1 .. floor (sqrt (n' / 2) - 1 / 2)],
        let i' = fromInteger i :: Double,
        j <- [i .. floor ((n' - i') / (2 * i' + 1))]
    ]

sundaram2 :: Integer -> [Integer]
sundaram2 n =
  foldr union [] [[i + j + 2 * i * j | j <- [i .. n]] | i <- [1 .. n]]
