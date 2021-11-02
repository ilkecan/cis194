module Stream where

data Stream a
  = Cons !a (Stream a)

instance Show a => Show (Stream a) where
  show = show . take 20 . streamToList

instance Eq a => Eq (Stream a) where
  (==) l r = take 20 (streamToList l) == take 20 (streamToList r)

instance Num a => Num (Stream a) where
  fromInteger n = Cons (fromInteger n) (streamRepeat 0)
  negate = streamMap negate
  (+) (Cons x xs) (Cons y ys) = Cons (x + y) (xs + ys)
  (*) (Cons x xs) (Cons y ys) = Cons (x * y) rest
    where
      rest = streamMap (* x) ys + streamMap (* y) xs + Cons 0 (xs * ys)
  abs = streamMap abs
  signum = streamMap signum

instance Integral a => Fractional (Stream a) where
  (/) (Cons 0 xs) (Cons 0 ys) = xs / ys
  (/) (Cons x xs) (Cons y ys) = result
    where
      result = Cons (x `div` y) rest
      rest = streamMap (`div` y) (xs - result * ys)
  fromRational = fromInteger . round

streamToList :: Stream a -> [a]
streamToList (Cons x xs) = x : streamToList xs

listToStream :: Num a => [a] -> Stream a
listToStream = foldr Cons (streamRepeat 0)

streamRepeat :: a -> Stream a
streamRepeat e = Cons e (streamRepeat e)

streamMap :: (a -> b) -> Stream a -> Stream b
streamMap f (Cons e xs) = Cons (f e) (streamMap f xs)

streamFromSeed :: (a -> a) -> a -> Stream a
streamFromSeed f seed = Cons seed (streamFromSeed f (f seed))
