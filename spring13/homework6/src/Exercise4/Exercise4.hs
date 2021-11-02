module Exercise4 where

import Stream (Stream (Cons))

streamRepeat :: a -> Stream a
streamRepeat e = Cons e (streamRepeat e)

streamMap :: (a -> b) -> Stream a -> Stream b
streamMap f (Cons e xs) = Cons (f e) (streamMap f xs)

streamFromSeed :: (a -> a) -> a -> Stream a
streamFromSeed f seed = Cons seed (streamFromSeed f (f seed))
