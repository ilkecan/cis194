module Exercise4 where

import Log
  ( KnownLogMessage,
    MessageTree (Leaf, Node),
  )

inOrder :: MessageTree -> [KnownLogMessage]
inOrder Leaf = []
inOrder (Node lmt msg rht) = inOrder lmt ++ msg : inOrder rht
