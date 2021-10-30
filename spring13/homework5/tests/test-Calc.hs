import Calc
  ( eval,
    evalStr,
  )
import Expr
  ( Expr (add, lit, mul),
    MinMax (MinMax),
    Mod7 (Mod7),
  )
import ExprT
  ( ExprT (Add, Lit, Mul),
  )
import Parser (parseExp)
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

testExp :: Expr a => Maybe a
testExp = parseExp lit add mul "(3 * -4) + 5"

unitTests :: TestTree
unitTests =
  testGroup
    "HUnit tests"
    [ -- Exercise1
      testCase "eval" $ eval (Mul (Add (Lit 2) (Lit 3)) (Lit 4)) @?= 20,
      -- Exercise2
      testCase "evalStr valid 1" $ evalStr "2" @?= Just 2,
      testCase "evalStr valid 2" $ evalStr "(2+3)*4" @?= Just 20,
      testCase "evalStr valid 3" $ evalStr "2+3*4" @?= Just 14,
      testCase "evalStr invalid" $ evalStr "2+3*" @?= Nothing,
      -- Exercise3
      testCase "Expr :: ExprT == ExprT" $
        (mul (add (lit 2) (lit 3)) (lit 4) :: ExprT)
          @?= Mul (Add (Lit 2) (Lit 3)) (Lit 4),
      -- Exercise4
      testCase "testExp :: Maybe Integer" $
        (testExp :: Maybe Integer) @?= Just (-7),
      testCase "testExp :: Maybe Bool" $
        (testExp :: Maybe Bool) @?= Just True,
      testCase "testExp :: Maybe MinMax" $
        (testExp :: Maybe MinMax) @?= Just (MinMax 5),
      testCase "testExp :: Maybe Mod7" $
        (testExp :: Maybe Mod7) @?= Just (Mod7 0)
    ]
