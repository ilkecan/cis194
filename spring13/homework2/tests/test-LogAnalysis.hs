import Log
  ( LogMessage (LogMessage, Unknown),
    MessageTree (Leaf, Node),
    MessageType (Error, Info, Warning),
  )
import LogAnalysis
  ( build,
    inOrder,
    insert,
    parseMessage,
    whatWentWrong,
  )
import Test.Tasty
import Test.Tasty.HUnit

-- sampleLogPath :: String
-- sampleLogPath = "../data/sample.log"

sampleLogMessages :: [LogMessage]
sampleLogMessages =
  [ LogMessage Info 6 "Completed armadillo processing",
    LogMessage Info 1 "Nothing to report",
    LogMessage (Error 99) 10 "Flange failed!",
    LogMessage Info 4 "Everything normal",
    LogMessage Info 11 "Initiating self-destruct sequence",
    LogMessage (Error 70) 3 "Way too many pickles",
    LogMessage (Error 65) 8 "Bad pickle-flange interaction detected",
    LogMessage Warning 5 "Flange is due for a check-up",
    LogMessage Info 7 "Out for lunch, back in two time steps",
    LogMessage (Error 20) 2 "Too many pickles",
    LogMessage Info 9 "Back from lunch"
  ]

sampleLogMessageTree :: MessageTree
sampleLogMessageTree =
  Node
    ( Node
        ( Node
            Leaf
            (Info, 1, "Nothing to report")
            Leaf
        )
        (Error 20, 2, "Too many pickles")
        ( Node
            ( Node
                ( Node
                    Leaf
                    (Error 70, 3, "Way too many pickles")
                    (Node Leaf (Info, 4, "Everything normal") Leaf)
                )
                (Warning, 5, "Flange is due for a check-up")
                (Node Leaf (Info, 6, "Completed armadillo processing") Leaf)
            )
            (Info, 7, "Out for lunch, back in two time steps")
            ( Node
                Leaf
                (Error 65, 8, "Bad pickle-flange interaction detected")
                Leaf
            )
        )
    )
    (Info, 9, "Back from lunch")
    ( Node
        ( Node
            Leaf
            (Error 99, 10, "Flange failed!")
            Leaf
        )
        (Info, 11, "Initiating self-destruct sequence")
        Leaf
    )

severeErrorMessages :: [String]
severeErrorMessages =
  [ "Way too many pickles",
    "Bad pickle-flange interaction detected",
    "Flange failed!"
  ]

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
      testCase "parseMessage \"\"" $ parseMessage "" @?= Unknown "",
      -- TODO: learn how to test `parse` using `testParse`
      -- Couldn't match expected type 'IO [LogMessage]'
      --             with actual type '[LogMessage]'
      -- testCase "testParse" $
      --   testParse parse 10 sampleLogPath @?= sampleLogMessages
      -- Exercise2
      testCase "insert Unknown" $ insert (Unknown "") Leaf @?= Leaf,
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
              @?= Node Leaf msg1 (Node Leaf msg2 Leaf),
      -- Exercise3
      testCase "build" $ build sampleLogMessages @?= sampleLogMessageTree,
      -- Exercise4
      testCase "inOrder" $
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
              ],
      -- Exercise5
      -- TODO: learn how to test `whatWentWrong` using `testWhatWentWrong`
      -- Couldn't match expected type 'IO [String]'
      --             with actual type '[String]'
      -- testCase "testWhatWentWrong" $
      --   testWhatWentWrong parse whatWentWrong sampleLogPath
      --     @?= severeErrorMessages,
      testCase "whatWentWrong" $
        whatWentWrong sampleLogMessages @?= severeErrorMessages
    ]
