module Exercise3 where

import Exercise2 (insert)
import Log
  ( LogMessage,
    MessageTree (Leaf),
  )

build :: [LogMessage] -> MessageTree
build = foldr insert Leaf
