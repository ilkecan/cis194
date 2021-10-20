import Exercise2
  ( doubleEveryOther1,
    doubleEveryOther2,
    doubleEveryOther3,
    doubleEveryOtherRev1,
    doubleEveryOtherRev2,
  )
import Test.Tasty.Bench

main :: IO ()
main =
  Test.Tasty.Bench.defaultMain
    [ bench "doubleEveryOtherRev1" $ nf doubleEveryOtherRev1 [8, 7, 6, 5],
      bench "doubleEveryOtherRev2" $ nf doubleEveryOtherRev2 [8, 7, 6, 5],
      bench "doubleEveryOther1" $ nf doubleEveryOther1 [8, 7, 6, 5],
      bench "doubleEveryOther2" $ nf doubleEveryOther2 [8, 7, 6, 5],
      bench "doubleEveryOther3" $ nf doubleEveryOther3 [8, 7, 6, 5]
    ]
