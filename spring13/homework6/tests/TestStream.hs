module TestStream where

import Data.Ratio ((%))
import Stream
  ( Stream (Cons),
    listToStream,
    streamFromSeed,
    streamMap,
    streamRepeat,
    streamToList,
  )
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck as QC

test_all :: TestTree
test_all = testGroup "TestStream" [qcProps, unitTests]

prop1 :: Int -> Bool
prop1 n = show stream == (show . replicate 20) n
  where
    stream = Cons n stream

prop2 :: Int -> Bool
prop2 n = (take 20 . streamToList . streamRepeat) n == replicate 20 n

prop3 :: Fun Int Int -> Int -> Bool
prop3 (Fn f) n = (streamMap f . streamRepeat) n == (streamRepeat . f) n

prop4 :: Fun Int Int -> Int -> Bool
prop4 (Fn f) n =
  (take 20 . streamToList . streamFromSeed f) n == (take 20 . iterate f) n

prop5 :: Fun Int Int -> Int -> Bool
prop5 (Fn f) n = streamFromSeed f n == (listToStream . iterate f) n

prop6 :: Fun Int Int -> Int -> Bool
prop6 (Fn f) n = streamFromSeed f n == streamFromSeed f n

prop7 :: Integer -> Bool
prop7 n = (take 1 . streamToList . fromInteger) n == [n]

prop8 :: Fun Integer Integer -> Integer -> Bool
prop8 (Fn f) n =
  (negate . streamFromSeed f) n == (streamMap negate . streamFromSeed f) n

prop9 :: Fun Integer Integer -> Integer -> Bool
prop9 (Fn f) n =
  (abs . streamFromSeed f) n == (streamMap abs . streamFromSeed f) n

prop10 :: Fun Integer Integer -> Integer -> Bool
prop10 (Fn f) n =
  (signum . streamFromSeed f) n == (streamMap signum . streamFromSeed f) n

qcProps :: TestTree
qcProps =
  testGroup
    "QuickCheck properties"
    [ QC.testProperty "show s@(Cons n s) == show . replicate 20 $ n" prop1,
      QC.testProperty
        "take 20 . streamToList . streamRepeat == replicate 20"
        prop2,
      QC.testProperty "streamMap f . streamRepeat == streamRepeat . f" prop3,
      QC.testProperty "streamToList . streamFromSeed f == iterate f" prop4,
      QC.testProperty "streamFromSeed f == listToStream . iterate f" prop5,
      QC.testProperty "streamFromSeed f n = streamFromSeed f n" prop6,
      QC.testProperty "(take 1 . streamToList . fromInteger n) = [n]" prop7,
      QC.testProperty
        "negate . streamFromSeed f == streamMap negate . streamFromSeed f"
        prop8,
      QC.testProperty
        "abs . streamFromSeed f == streamMap abs . streamFromSeed f"
        prop9,
      QC.testProperty
        "signum . streamFromSeed f == streamMap signum . streamFromSeed f"
        prop10
    ]

unitTests :: TestTree
unitTests =
  testGroup
    "HUnit tests"
    [ testCase "fromRational integral" $
        fromRational 2 @?= listToStream [2 :: Integer],
      testCase "fromRational round down" $
        fromRational (13 % 4) @?= listToStream [3 :: Integer],
      testCase "fromRational round up" $
        fromRational (15 % 4) @?= listToStream [4 :: Integer]
    ]
