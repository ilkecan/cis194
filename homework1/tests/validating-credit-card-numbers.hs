import           Data.Char                      ( digitToInt )
import           Test.Tasty
import           Test.Tasty.HUnit
import           Test.Tasty.QuickCheck         as QC
import           ValidatingCreditCardNumbers    ( doubleEveryOther
                                                , toDigits
                                                , toDigitsRev
                                                )

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [properties, unitTests]

properties :: TestTree
properties = testGroup "Properties" [qcProps]

prop_1 :: Integer -> Bool
prop_1 number = toDigits number == (reverse . toDigitsRev) number

prop_2 :: QC.Positive Integer -> Bool
prop_2 (QC.Positive number) =
  toDigits number == map (toInteger . digitToInt) (show number)

prop_3 :: [Integer] -> Bool
prop_3 list = (length . doubleEveryOther) list == length list

qcProps :: TestTree
qcProps = testGroup
  "QuickCheck properties"
  [ QC.testProperty "toDigits == reverse . toDigitsRev" prop_1
  , QC.testProperty
    "toDigits number == map (toInteger . digitToInt) (show number) where number > 0"
    prop_2
  , QC.testProperty "length . doubleEveryOther == length" prop_3
  ]

unitTests :: TestTree
unitTests = testGroup
  "HUnit tests"
  [ testCase "toDigits" $ toDigits 1234 @?= [1, 2, 3, 4]
  , testCase "toDigitsRev" $ toDigitsRev 1234 @?= [4, 3, 2, 1]
  , testCase "toDigits 0" $ toDigits 0 @?= []
  , testCase "toDigits negative" $ toDigits (-17) @?= []
  , testCase "doubleEveryOther empty" $ doubleEveryOther [] @?= []
  , testCase "doubleEveryOther single" $ doubleEveryOther [3] @?= [3]
  , testCase "doubleEveryOther even"
  $   doubleEveryOther [8, 7, 6, 5]
  @?= [16, 7, 12, 5]
  , testCase "doubleEveryOther odd" $ doubleEveryOther [1, 2, 3] @?= [1, 4, 3]
  ]
