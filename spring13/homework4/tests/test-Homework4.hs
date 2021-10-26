import Data.Maybe (isJust)
import Homework4
  ( foldTree,
    fun1,
    fun2,
  )
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck as QC
import Tree
  ( Height,
    Tree (Leaf, Node),
    getHeight,
  )

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [properties, unitTests]

properties :: TestTree
properties = testGroup "Properties" [qcProps]

fun1Baseline :: [Integer] -> Integer
fun1Baseline [] = 1
fun1Baseline (x : xs)
  | even x = (x - 2) * fun1Baseline xs
  | otherwise = fun1Baseline xs

prop_1 :: [Integer] -> Bool
prop_1 ns = fun1 ns == fun1Baseline ns

fun2Baseline :: Integer -> Integer
fun2Baseline 1 = 0
fun2Baseline n
  | even n = n + fun2Baseline (n `div` 2)
  | otherwise = fun2Baseline (3 * n + 1)

prop_2 :: QC.Positive Integer -> Bool
prop_2 (QC.Positive n) = fun2 n == fun2Baseline n

isTreeHeightCorrect :: Tree a -> Bool
isTreeHeightCorrect = isJust . treeHeight

treeHeight :: Tree a -> Maybe Height
treeHeight Leaf = Just $ -1
treeHeight (Node height left _ right) =
  case (treeHeight left, treeHeight right) of
    (Just x, Just y)
      | height == max x y + 1 -> Just height
      | otherwise -> Nothing
    _wrongSubTreeHeight -> Nothing

prop_3 :: [Integer] -> Bool
prop_3 = isTreeHeightCorrect . foldTree

isTreeBalanced :: Tree a -> Bool
isTreeBalanced Leaf = True
isTreeBalanced (Node _ left _ right) =
  subTreeHeightDifference <= 1 && all isTreeBalanced [left, right]
  where
    subTreeHeightDifference = abs (getHeight left - getHeight right)

prop_4 :: [Integer] -> Bool
prop_4 = isTreeBalanced . foldTree

qcProps :: TestTree
qcProps =
  testGroup
    "QuickCheck properties"
    [ QC.testProperty "fun1 == fun1Baseline " prop_1,
      QC.testProperty "fun2 == fun2Baseline" prop_2,
      QC.testProperty "isTreeHeightCorrect . foldTree" prop_3,
      QC.testProperty "isTreeBalanced . foldTree" prop_4
    ]

unitTests :: TestTree
unitTests =
  testGroup
    "HUnit tests"
    [ -- Exercise1
      testCase "fun1 []" $ fun1 [] @?= 1,
      testCase "fun2 1" $ fun2 1 @?= 0,
      -- Exercise2
      testCase "foldTree []" $ foldTree ([] :: [Int]) @?= Leaf,
      testCase "foldTree [1]" $ foldTree [1 :: Int] @?= Node 0 Leaf 1 Leaf
    ]
