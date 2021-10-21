import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck as QC
import TheTowersOfHanoi
  ( hanoi,
  )

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [properties, unitTests]

properties :: TestTree
properties = testGroup "Properties" [qcProps]

prop_1 :: Gen Bool
prop_1 = do
  number <- choose (0, 10)
  return $ length (hanoi number "a" "b" "c") == 2 ^ number - 1

qcProps :: TestTree
qcProps =
  testGroup
    "QuickCheck properties"
    [ QC.testProperty "length . hanoi n == 2^n - 1" prop_1
    ]

unitTests :: TestTree
unitTests =
  testGroup
    "HUnit tests"
    [ testCase "hanoi zero" $ hanoi 0 "a" "b" "c" @?= [],
      testCase "hanoi one" $ hanoi 1 "a" "b" "c" @?= [("a", "b")],
      testCase "hanoi" $ hanoi 2 "a" "b" "c" @?= [("a", "c"), ("a", "b"), ("c", "b")]
    ]
