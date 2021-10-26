-- Folding with trees
module Exercise2 where

import Data.List (foldl')
import Tree
  ( Tree (Leaf, Node),
    getHeight,
  )

foldTree :: [a] -> Tree a
foldTree = foldl' insertToTree Leaf

insertToTree :: Tree a -> a -> Tree a
insertToTree Leaf x = Node 0 Leaf x Leaf
insertToTree (Node _ left d right) x
  | getHeight right < getHeight left =
    let newRight = insertToTree right x
        newHeight = (maximum . map getHeight) [left, newRight] + 1
     in Node newHeight left d newRight
  | otherwise =
    let newLeft = insertToTree left x
        newHeight = (maximum . map getHeight) [newLeft, right] + 1
     in Node newHeight newLeft d right
