import Golf
  ( skips,
  )
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck as QC

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [properties, unitTests]

properties :: TestTree
properties = testGroup "Properties" [qcProps]

prop_1 :: [Int] -> Bool
prop_1 list = (length . skips) list == length list

qcProps :: TestTree
qcProps =
  testGroup
    "QuickCheck properties"
    [ QC.testProperty "lenght . skips = length" prop_1
    ]

unitTests :: TestTree
unitTests =
  testGroup
    "HUnit tests"
    [ -- Exercise1
      testCase "skips String 1" $ skips "ABCD" @?= ["ABCD", "BD", "C", "D"],
      testCase "skips String 2" $
        skips "hello!" @?= ["hello!", "el!", "l!", "l", "o", "!"],
      testCase "skips [Int]" $ skips [1 :: Integer] @?= [[1]],
      testCase "skips [Bool]" $
        skips [True, False] @?= [[True, False], [False]],
      testCase "skips []" $ skips ([] :: [Integer]) @?= []
    ]
