module Exercise2 where

import Log
  ( LogMessage (LogMessage, Unknown),
    MessageTree (Leaf, Node),
  )

insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) mt = mt
insert msg Leaf = Node Leaf msg Leaf
insert msg1@(LogMessage _ ts1 _) (Node lmt msg2@(LogMessage _ ts2 _) rmt)
  | ts1 < ts2 = Node (insert msg1 lmt) msg2 rmt
  | otherwise = Node lmt msg2 (insert msg1 rmt)
insert _ (Node _ (Unknown _) _) =
  error "MessageTree can't contain an Unknown message"
