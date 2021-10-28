-- More folds!
module Exercise3 where

import Data.List (foldl')

xor :: [Bool] -> Bool
xor = foldl' (\x y -> if y then not x else x) False

map' :: (a -> b) -> [a] -> [b]
map' f = foldr ((:) . f) []

-- only valid for List's
myFoldl' :: (b -> a -> b) -> b -> [a] -> b
myFoldl' f base xs = foldr (flip f) base (reverse xs)

-- https://www.hacklewayne.com/foldl-in-terms-of-foldr
-- https://scturtle.me/posts/2016-01-27-foldl.html
-- myFoldl f base [0..9] =
--   (\x0 -> ( ... (\x8 -> (\x9 -> id (f x9 9)) (f x8 8)) ... ) (f x0 0)) base
myFoldl :: Foldable t => (b -> a -> b) -> b -> t a -> b
myFoldl f base list =
  let g listElem accFunc acc = accFunc (f acc listElem)
      baseFunc = id
   in foldr g baseFunc list base
