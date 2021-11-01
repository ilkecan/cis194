module TestData where

import Log
  ( LogMessage (LogMessage),
    MessageTree (Leaf, Node),
    MessageType (Error, Info, Warning),
  )

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
