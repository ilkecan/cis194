module TestExercise3 where

import Data.List
  ( group,
    sort,
  )
import Exercise3 (histogram)
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck as QC

test_all :: TestTree
test_all = testGroup "TestExercise3" [qcProps, unitTests]

digitList :: Gen [Integer]
digitList = listOf $ chooseInteger (0, 9)

prop1 :: [Integer] -> Bool
prop1 list = (length . histogram) list == 11 * (height + 2)
  where
    height =
      if null list
        then 0
        else (maximum . map length . group . sort) list

qcProps :: TestTree
qcProps =
  testGroup
    "QuickCheck properties"
    [ QC.testProperty "length . histogram == 11 * (height + 2)" $
        forAll digitList prop1
    ]

histogram1Output :: String
histogram1Output = "   * *    \n==========\n0123456789\n"

histogram2Output :: String
histogram2Output =
  " *        \n *        \n *   *    \n==========\n0123456789\n"

histogram3Output :: String
histogram3Output =
  "    *     \n    *     \n    * *   \n ******  *\n==========\n0123456789\n"

unitTests :: TestTree
unitTests =
  testGroup
    "HUnit tests"
    [ testCase "histogram 1" $ histogram [3, 5] @?= histogram1Output,
      testCase "histogram 2" $ histogram [1, 1, 1, 5] @?= histogram2Output,
      testCase "histogram 3" $
        histogram [1, 4, 5, 4, 6, 6, 3, 4, 2, 4, 9] @?= histogram3Output,
      testCase "histogram []" $ histogram [] @?= "==========\n0123456789\n"
    ]
