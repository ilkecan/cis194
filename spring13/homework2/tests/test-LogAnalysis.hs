import Log
  ( LogMessage (LogMessage, Unknown),
    MessageTree (Leaf, Node),
    MessageType (Error, Info, Warning),
  )
import LogAnalysis
  ( build,
    insert,
    parseMessage,
  )
import Test.Tasty
import Test.Tasty.HUnit

sampleLogMessages :: [LogMessage]
sampleLogMessages =
  [ LogMessage Info 6 "Completed armadillo processing",
    LogMessage Info 1 "Nothing to report",
    LogMessage Info 4 "Everything normal",
    LogMessage Info 11 "Initiating self-destruct sequence",
    LogMessage (Error 70) 3 "Way too many pickles",
    LogMessage (Error 65) 8 "Bad pickle-flange interaction detected",
    LogMessage Warning 5 "Flange is due for a check-up",
    LogMessage Info 7 "Out for lunch, back in two time steps",
    LogMessage (Error 20) 2 "Too many pickles",
    LogMessage Info 9 "Back from lunch"
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
      --   testParse parse 10 "../data/sample.log" @?= sampleLogMessages
      -- Exercise2
      testCase "insert Unknown" $ insert (Unknown "") Leaf @?= Leaf,
      let mt = Node Leaf (LogMessage Info 1 "") Leaf
       in testCase "insert to left" $ insert (Unknown "") mt @?= mt,
      let m1 = LogMessage Info 2 ""
          m2 = LogMessage Info 1 ""
          mt = Node Leaf m1 Leaf
       in testCase "insert to right" $
            insert m2 mt @?= Node (Node Leaf m2 Leaf) m1 Leaf,
      let m1 = LogMessage Info 2 ""
          m2 = LogMessage Info 3 ""
          mt = Node Leaf m1 Leaf
       in testCase "insert Unknown 2" $
            insert m2 mt @?= Node Leaf m1 (Node Leaf m2 Leaf),
      -- Exercise3
      testCase "build" $
        build sampleLogMessages
          @?= Node
            ( Node
                (Node Leaf (LogMessage Info 1 "Nothing to report") Leaf)
                (LogMessage (Error 20) 2 "Too many pickles")
                ( Node
                    ( Node
                        ( Node
                            Leaf
                            (LogMessage (Error 70) 3 "Way too many pickles")
                            ( Node
                                Leaf
                                (LogMessage Info 4 "Everything normal")
                                Leaf
                            )
                        )
                        (LogMessage Warning 5 "Flange is due for a check-up")
                        ( Node
                            Leaf
                            (LogMessage Info 6 "Completed armadillo processing")
                            Leaf
                        )
                    )
                    (LogMessage Info 7 "Out for lunch, back in two time steps")
                    ( Node
                        Leaf
                        ( LogMessage
                            (Error 65)
                            8
                            "Bad pickle-flange interaction detected"
                        )
                        Leaf
                    )
                )
            )
            (LogMessage Info 9 "Back from lunch")
            ( Node
                Leaf
                (LogMessage Info 11 "Initiating self-destruct sequence")
                Leaf
            )
    ]
