module Stream where

data Stream a
  = Cons !a (Stream a)

instance Show a => Show (Stream a) where
  show = show . take 20 . streamToList

instance Eq a => Eq (Stream a) where
  (==) l r = take 20 (streamToList l) == take 20 (streamToList r)

streamToList :: Stream a -> [a]
streamToList (Cons x xs) = x : streamToList xs
