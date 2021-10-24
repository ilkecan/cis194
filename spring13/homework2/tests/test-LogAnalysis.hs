import Log
  ( LogMessage (LogMessage, Unknown),
    MessageType (Error, Info, Warning),
  )
import LogAnalysis
  ( parseMessage,
  )
import Test.Tasty
import Test.Tasty.HUnit

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [unitTests]

unitTests :: TestTree
unitTests =
  testGroup
    "HUnit tests"
    [ -- Exercise1
      testCase "parseMessage Info" $
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
      --   testParse parse 10 "../data/sample.log"
      --     @?= [ LogMessage Info 6 "Completed armadillo processing",
      --           LogMessage Info 1 "Nothing to report",
      --           LogMessage Info 4 "Everything normal",
      --           LogMessage Info 11 "Initiating self-destruct sequence",
      --           LogMessage (Error 70) 3 "Way too many pickles",
      --           LogMessage
      --             (Error 65)
      --             8
      --             "Bad pickle-flange interaction detected",
      --           LogMessage Warning 5 "Flange is due for a check-up",
      --           LogMessage Info 7 "Out for lunch, back in two time steps",
      --           LogMessage (Error 20) 2 "Too many pickles",
      --           LogMessage Info 9 "Back from lunch"
      --         ],
    ]
