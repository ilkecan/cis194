module Exercise4 where

import Log
  ( LogMessage,
    MessageTree (Leaf, Node),
  )

inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder (Node lmt msg rht) = inOrder lmt ++ msg : inOrder rht
