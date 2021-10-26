import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck as QC
import TheTowersOfHanoi
  ( hanoi,
    hanoi4,
  )

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [properties, unitTests]

properties :: TestTree
properties = testGroup "Properties" [qcProps]

prop_1 :: Integer -> Bool
prop_1 n = length (hanoi n "a" "b" "c") == 2 ^ n - 1

-- from Table1 of
-- https://service.scs.carleton.ca/sites/default/files/tr/TR-04-10.pdf
hanoi4MinimumNumberOfMoves :: [Int]
hanoi4MinimumNumberOfMoves =
  [ 0,
    1,
    3,
    5,
    9,
    13,
    17,
    25,
    33,
    41,
    49,
    65,
    81,
    97,
    113,
    129,
    161,
    193,
    225,
    257,
    289
  ]

prop_2 :: Integer -> Bool
prop_2 n =
  length (hanoi4 n "a" "b" "c" "d")
    == hanoi4MinimumNumberOfMoves !! fromIntegral n

qcProps :: TestTree
qcProps =
  testGroup
    "QuickCheck properties"
    [ QC.testProperty "length . hanoi n == 2^n - 1" $
        forAll (chooseInteger (0, 10)) prop_1,
      QC.testProperty
        "length . hanoi4 n == hanoi4MinimumNumberOfMoves !! n"
        $ forAll (chooseInteger (0, 20)) prop_2
    ]

unitTests :: TestTree
unitTests =
  testGroup
    "HUnit tests"
    [ testCase "hanoi 0" $ hanoi 0 "a" "b" "c" @?= [],
      testCase "hanoi 1" $ hanoi 1 "a" "b" "c" @?= [("a", "b")],
      testCase "hanoi 2" $
        hanoi 2 "a" "b" "c" @?= [("a", "c"), ("a", "b"), ("c", "b")],
      testCase "hanoi4 0" $ hanoi4 0 "a" "b" "c" "d" @?= [],
      testCase "hanoi4 1" $ hanoi4 1 "a" "b" "c" "d" @?= [("a", "b")],
      testCase "hanoi4 2" $
        hanoi4 2 "a" "b" "c" "d" @?= [("a", "c"), ("a", "b"), ("c", "b")],
      testCase "length . hanoi4 15 == 129" $
        length (hanoi4 15 "a" "b" "c" "d") @?= 129
    ]
