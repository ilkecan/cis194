module Exercise2 where

import Log
  ( LogMessage (LogMessage, Unknown),
    MessageTree (Leaf, Node),
  )

insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) mt = mt
insert (LogMessage mt ts str) Leaf = Node Leaf (mt, ts, str) Leaf
insert msg1@(LogMessage _ ts1 _) (Node left msg2@(_, ts2, _) right)
  | ts1 < ts2 = Node (insert msg1 left) msg2 right
  | otherwise = Node left msg2 (insert msg1 right)
