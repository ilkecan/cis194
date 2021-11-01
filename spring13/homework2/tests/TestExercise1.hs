module TestExercise1 where

import Exercise1 (parseMessage)
import Log
  ( LogMessage (LogMessage, Unknown),
    MessageType (Error, Info, Warning),
  )
import Test.Tasty
import Test.Tasty.HUnit

test_all :: TestTree
test_all = testGroup "TestExercise1" [unitTests]

-- sampleLogPath :: String
-- sampleLogPath = "../data/sample.log"

unitTests :: TestTree
unitTests =
  testGroup
    "HUnit tests"
    [ testCase "parseMessage Info" $
        parseMessage
          "I 147 mice in the air, I'm afraid, but you might catch a bat, and"
          @?= LogMessage
            Info
            147
            "mice in the air, I'm afraid, but you might catch a bat, and",
      testCase "parseMessage Warning" $
        parseMessage "W 562 help help"
          @?= LogMessage Warning 562 "help help",
      testCase "parseMessage Error" $
        parseMessage "E 2 148 #56k istereadeat lo d200ff] BOOTMEM"
          @?= LogMessage (Error 2) 148 "#56k istereadeat lo d200ff] BOOTMEM",
      testCase "parseMessage Unknown" $
        parseMessage "This is not in the right format"
          @?= Unknown "This is not in the right format",
      testCase "parseMessage \"\"" $ parseMessage "" @?= Unknown ""
      -- TODO: learn how to test `parse` using `testParse`
      -- Couldn't match expected type 'IO [LogMessage]'
      --             with actual type '[LogMessage]'
      -- testCase "testParse" $
      --   testParse parse 10 sampleLogPath @?= sampleLogMessages
    ]
