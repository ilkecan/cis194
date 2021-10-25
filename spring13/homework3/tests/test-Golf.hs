import Golf
  ( localMaxima,
    skips,
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

prop_2 :: [Integer] -> Bool
prop_2 list = (length . localMaxima) list <= length list

qcProps :: TestTree
qcProps =
  testGroup
    "QuickCheck properties"
    [ QC.testProperty "length . skips = length" prop_1,
      QC.testProperty "length . localMaxima = length" prop_2
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
      testCase "skips []" $ skips ([] :: [Integer]) @?= [],
      -- Exercise2
      testCase "localMaxima multiple" $ localMaxima [2, 9, 5, 6, 1] @?= [9, 6],
      testCase "localMaxima single" $ localMaxima [2, 3, 4, 1, 5] @?= [4],
      testCase "localMaxima none" $ localMaxima [1, 2, 3, 4, 5] @?= [],
      testCase "localMaxima []" $ localMaxima [] @?= [],
      testCase "localMaxima [1]" $ localMaxima [1] @?= [],
      testCase "localMaxima [1,2]" $ localMaxima [1, 2] @?= []
    ]
