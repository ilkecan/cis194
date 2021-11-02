module TestExercise4 where

import Exercise4
  ( streamFromSeed,
    streamMap,
    streamRepeat,
  )
import Stream (streamToList)
import Test.Tasty
import Test.Tasty.QuickCheck as QC

test_all :: TestTree
test_all = testGroup "TestExercise4" [qcProps]

prop1 :: Fun Int Int -> Int -> Bool
prop1 (Fn f) n = (streamMap f . streamRepeat) n == (streamRepeat . f) n

prop2 :: Fun Int Int -> Int -> Bool
prop2 (Fn f) n =
  (take 20 . streamToList . streamFromSeed f) n == (take 20 . iterate f) n

qcProps :: TestTree
qcProps =
  testGroup
    "QuickCheck properties"
    [ QC.testProperty "streamMap f . streamRepeat == streamRepeat . f" prop1,
      QC.testProperty "streamToList . streamFromSeed f == iterate f" prop2
    ]
