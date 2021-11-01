module TestExercise2 where

import Exercise2 (insert)
import Log
  ( LogMessage (LogMessage, Unknown),
    MessageTree (Leaf, Node),
    MessageType (Info),
  )
import Test.Tasty
import Test.Tasty.HUnit

test_all :: TestTree
test_all = testGroup "TestExercise2" [unitTests]

unitTests :: TestTree
unitTests =
  testGroup
    "HUnit tests"
    [ testCase "insert Unknown" $ insert (Unknown "") Leaf @?= Leaf,
      let mt = Node Leaf (Info, 1, "") Leaf
       in testCase "insert to left" $ insert (Unknown "") mt @?= mt,
      let msg1 = (Info, 2, "")
          msg2 = (Info, 1, "")
          mt = Node Leaf msg1 Leaf
       in testCase "insert to right" $
            insert (LogMessage Info 1 "") mt
              @?= Node (Node Leaf msg2 Leaf) msg1 Leaf,
      let msg1 = (Info, 2, "")
          msg2 = (Info, 3, "")
          mt = Node Leaf msg1 Leaf
       in testCase "insert Unknown 2" $
            insert (LogMessage Info 3 "") mt
              @?= Node Leaf msg1 (Node Leaf msg2 Leaf)
    ]
