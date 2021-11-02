{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- Fibonacci numbers via generating functions
module Exercise6 where

import Exercise4
  ( streamMap,
    streamRepeat,
  )
import Stream (Stream (Cons))

x :: Stream Integer
x = Cons 0 (Cons 1 (streamRepeat 0))

instance Num (Stream Integer) where
  fromInteger n = Cons n (streamRepeat 0)
  negate = streamMap negate
  (+) (Cons x' xs) (Cons y ys) = Cons (x' + y) (xs + ys)
  (*) (Cons x' xs) (Cons y ys) = Cons (x' * y) rest
    where
      rest = streamMap (* x') ys + streamMap (* y) xs + Cons 0 (xs * ys)
  abs = streamMap abs
  signum = streamMap signum
