module TestExercise3 where

import Exercise3
  ( map',
    myFoldl,
    myFoldl',
    xor,
  )
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck as QC

test_all :: TestTree
test_all = testGroup "TestExercise3" [qcProps, unitTests]

prop1 :: [Bool] -> Bool
prop1 list = xor list == (odd . length . filter id) list

prop2 :: Fun Int Int -> [Int] -> Bool
prop2 (Fn f) list = map' f list == map f list

type FoldFunc = ([Int] -> Int -> [Int]) -> [Int] -> [Int] -> [Int]

prop3 :: FoldFunc -> Fun ([Int], Int) [Int] -> [Int] -> Bool
prop3 fold (Fn2 f) acc = fold f acc [] == acc

prop4 :: FoldFunc -> [Int] -> Bool
prop4 fold list = fold (flip (:)) [] list == reverse list

qcProps :: TestTree
qcProps =
  testGroup
    "QuickCheck properties"
    [ QC.testProperty "xor == odd . length . filter id" prop1,
      QC.testProperty "map' == map" prop2,
      -- TODO: Is there a way to avoid this duplication i.e., to test prop3
      -- and prop4 for the both fold functions in a more idiomatic way?
      QC.testProperty "myFoldl' f acc [] = acc" $ prop3 myFoldl',
      QC.testProperty "myFoldl' (flip (:)) [] == reverse" $ prop3 myFoldl',
      QC.testProperty "myFoldl f acc [] = acc" $ prop4 myFoldl,
      QC.testProperty "myFoldl (flip (:)) [] == reverse" $ prop4 myFoldl
    ]

unitTests :: TestTree
unitTests =
  testGroup
    "HUnit tests"
    [ testCase "xor []" $ xor [] @?= False,
      testCase "map' (+2) [0..9] == map (+2) [0..9]" $
        map' (+ 2) [0 .. 9 :: Int] @?= map (+ 2) [0 .. 9],
      testCase "myFoldl (flip (:)) [] == reverse" $
        myFoldl (flip (:)) [] [0 .. 9 :: Int] @?= [9, 8 .. 0]
    ]
