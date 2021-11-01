module TestExercise5 where

import Exercise5 (whatWentWrong)
import Test.Tasty
import Test.Tasty.HUnit
import TestData (sampleLogMessages)

test_all :: TestTree
test_all = testGroup "TestExercise5" [unitTests]

-- sampleLogPath :: String
-- sampleLogPath = "../data/sample.log"

severeErrorMessages :: [String]
severeErrorMessages =
  [ "Way too many pickles",
    "Bad pickle-flange interaction detected",
    "Flange failed!"
  ]

unitTests :: TestTree
unitTests =
  testGroup
    "HUnit tests"
    [ -- TODO: learn how to test `whatWentWrong` using `testWhatWentWrong`
      -- Couldn't match expected type 'IO [String]'
      --             with actual type '[String]'
      -- testCase "testWhatWentWrong" $
      --   testWhatWentWrong parse whatWentWrong sampleLogPath
      --     @?= severeErrorMessages,
      testCase "whatWentWrong" $
        whatWentWrong sampleLogMessages @?= severeErrorMessages
    ]
