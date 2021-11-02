module TestExercise6 where

import Exercise4
  ( streamFromSeed,
    streamMap,
  )
import Exercise6
  ( x,
  )
import Stream (streamToList)
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck as QC

test_all :: TestTree
test_all = testGroup "TestExercise6" [qcProps, unitTests]

prop1 :: Integer -> Bool
prop1 n = (take 1 . streamToList . fromInteger) n == [n]

prop2 :: Fun Integer Integer -> Integer -> Bool
prop2 (Fn f) n =
  (negate . streamFromSeed f) n == (streamMap negate . streamFromSeed f) n

prop3 :: Fun Integer Integer -> Integer -> Bool
prop3 (Fn f) n =
  (abs . streamFromSeed f) n == (streamMap abs . streamFromSeed f) n

prop4 :: Fun Integer Integer -> Integer -> Bool
prop4 (Fn f) n =
  (signum . streamFromSeed f) n == (streamMap signum . streamFromSeed f) n

qcProps :: TestTree
qcProps =
  testGroup
    "QuickCheck properties"
    [ QC.testProperty "(take 1 . streamToList . fromInteger n) = [n]" prop1,
      QC.testProperty
        "negate . streamFromSeed f == streamMap negate . streamFromSeed f"
        prop2,
      QC.testProperty
        "abs . streamFromSeed f == streamMap abs . streamFromSeed f"
        prop3,
      QC.testProperty
        "signum . streamFromSeed f == streamMap signum . streamFromSeed f"
        prop4
    ]

unitTests :: TestTree
unitTests =
  testGroup
    "HUnit tests"
    [ testCase "show x" $
        show x @?= (show . take 20) (0 : 1 : repeat (0 :: Int))
    ]
