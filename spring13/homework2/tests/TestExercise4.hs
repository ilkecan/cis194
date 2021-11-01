module TestExercise4 where

import Exercise4 (inOrder)
import Log (MessageType (Error, Info, Warning))
import Test.Tasty
import Test.Tasty.HUnit
import TestData (sampleLogMessageTree)

test_all :: TestTree
test_all = testGroup "TestExercise4" [unitTests]

unitTests :: TestTree
unitTests =
  testGroup
    "HUnit tests"
    [ testCase "inOrder" $
        inOrder sampleLogMessageTree
          @?= [ (Info, 1, "Nothing to report"),
                (Error 20, 2, "Too many pickles"),
                (Error 70, 3, "Way too many pickles"),
                (Info, 4, "Everything normal"),
                (Warning, 5, "Flange is due for a check-up"),
                (Info, 6, "Completed armadillo processing"),
                (Info, 7, "Out for lunch, back in two time steps"),
                (Error 65, 8, "Bad pickle-flange interaction detected"),
                (Info, 9, "Back from lunch"),
                (Error 99, 10, "Flange failed!"),
                (Info, 11, "Initiating self-destruct sequence")
              ]
    ]
