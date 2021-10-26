module Tree where

type Height = Integer

data Tree a
  = Leaf
  | Node !Height (Tree a) !a (Tree a)
  deriving (Show, Eq)

getHeight :: Tree a -> Height
getHeight Leaf = -1
getHeight (Node h _ _ _) = h
