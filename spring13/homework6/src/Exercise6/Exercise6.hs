-- Fibonacci numbers via generating functions
module Exercise6 where

import Stream
  ( Stream (Cons),
    streamRepeat,
  )

x :: Stream Integer
x = Cons 0 (Cons 1 (streamRepeat 0))

fibs3 :: Stream Integer
fibs3 = x / (1 - x - x ^ (2 :: Int))
