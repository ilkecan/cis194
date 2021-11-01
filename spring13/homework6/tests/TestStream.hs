module TestStream where

import Stream (Stream (Cons))
import Test.Tasty
import Test.Tasty.QuickCheck as QC

test_all :: TestTree
test_all = testGroup "TestStream" [qcProps]

prop1 :: Int -> Bool
prop1 n = show stream == (show . replicate 20) n
  where
    stream = Cons n stream

qcProps :: TestTree
qcProps =
  testGroup
    "QuickCheck properties"
    [ QC.testProperty "show s@(Cons n s) == show . replicate 20 $ n" prop1
    ]
