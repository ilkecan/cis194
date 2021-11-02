module Exercise5 where

import Exercise4
  ( streamFromSeed,
    streamRepeat,
  )
import Stream (Stream (Cons))

nats :: Stream Integer
nats = streamFromSeed (+ 1) 0

ruler :: Stream Integer
ruler = ruler' 0
  where
    ruler' n = interleaveStreams (streamRepeat n) (ruler' (n + 1))

interleaveStreams :: Stream a -> Stream a -> Stream a
interleaveStreams (Cons x xs) ys = Cons x (interleaveStreams ys xs)

-- the implementation below prevents `ruler'` from being tail recursion and
-- creates an infinite recursion and finally causes a stack overflow
-- interleaveStreams (Cons x xs) (Cons y ys) =
--   Cons x (Cons y (interleaveStreams xs ys))
