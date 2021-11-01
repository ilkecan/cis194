module TestExercise5 where

import Exercise5 (compile)
import StackVM
  ( StackExp (Add, Mul, PushI),
    StackVal (IVal),
    stackVM,
  )
import Test.Tasty
import Test.Tasty.HUnit

test_all :: TestTree
test_all = testGroup "TestExercise5" [unitTests]

unitTests :: TestTree
unitTests =
  testGroup
    "HUnit tests"
    [ testCase "stackVM" $
        stackVM [PushI 3, PushI (-4), Mul, PushI 5, Add] @?= Right (IVal (-7)),
      testCase "compile 1" $ compile "3 + 5" @?= Just [PushI 3, PushI 5, Add],
      testCase "compile 2" $
        compile "(3 * -4) + 5" @?= Just [PushI 3, PushI (-4), Mul, PushI 5, Add]
    ]
