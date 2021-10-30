import Calc
  ( eval,
  )
import ExprT
  ( ExprT (Add, Lit, Mul),
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

prop_1 :: Integer -> Bool
prop_1 n = eval (Lit n) == n

prop_2 :: Integer -> Integer -> Bool
prop_2 n1 n2 = eval (Add (Lit n1) (Lit n2)) == n1 + n2

prop_3 :: Integer -> Integer -> Bool
prop_3 n1 n2 = eval (Mul (Lit n1) (Lit n2)) == n1 * n2

qcProps :: TestTree
qcProps =
  testGroup
    "QuickCheck properties"
    [ QC.testProperty "eval (Lit n) == n" prop_1,
      QC.testProperty "eval (Add (Lit n1) (Lit n2)) == n1 + n2" prop_2,
      QC.testProperty "eval (Mul (Lit n1) (Lit n2)) == n1 * n2" prop_3
    ]

unitTests :: TestTree
unitTests =
  testGroup
    "HUnit tests"
    [ -- Exercise1
      testCase "eval" $ eval (Mul (Add (Lit 2) (Lit 3)) (Lit 4)) @?= 20
    ]
