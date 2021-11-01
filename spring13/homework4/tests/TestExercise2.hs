module TestExercise2 where

import Data.Maybe (isJust)
import Exercise2 (foldTree)
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck as QC
import Tree
  ( Height,
    Tree (Leaf, Node),
    getHeight,
  )

test_all :: TestTree
test_all = testGroup "TestExercise2" [qcProps, unitTests]

treeHeight :: Tree a -> Maybe Height
treeHeight Leaf = Just $ -1
treeHeight (Node height left _ right) =
  case (treeHeight left, treeHeight right) of
    (Just x, Just y)
      | height == max x y + 1 -> Just height
      | otherwise -> Nothing
    _wrongSubTreeHeight -> Nothing

prop1 :: [Integer] -> Bool
prop1 = isJust . treeHeight . foldTree

isTreeBalanced :: Tree a -> Bool
isTreeBalanced Leaf = True
isTreeBalanced (Node _ left _ right) =
  subTreeHeightDifference <= 1 && all isTreeBalanced [left, right]
  where
    subTreeHeightDifference = abs (getHeight left - getHeight right)

prop2 :: [Integer] -> Bool
prop2 = isTreeBalanced . foldTree

qcProps :: TestTree
qcProps =
  testGroup
    "QuickCheck properties"
    [ QC.testProperty "isTreeHeightCorrect . foldTree" prop1,
      QC.testProperty "isTreeBalanced . foldTree" prop2
    ]

unitTests :: TestTree
unitTests =
  testGroup
    "HUnit tests"
    [ testCase "foldTree []" $ foldTree ([] :: [Int]) @?= Leaf,
      testCase "foldTree [1]" $ foldTree [1 :: Int] @?= Node 0 Leaf 1 Leaf
    ]
