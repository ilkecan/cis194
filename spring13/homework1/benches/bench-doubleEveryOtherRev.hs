import Exercise2
  ( doubleEveryOtherRev1,
    doubleEveryOtherRev2,
  )
import Test.Tasty.Bench

main :: IO ()
main =
  Test.Tasty.Bench.defaultMain
    [ bench "doubleEveryOtherRev1" $ nf doubleEveryOtherRev1 [8, 7, 6, 5],
      bench "doubleEveryOtherRev2" $ nf doubleEveryOtherRev2 [8, 7, 6, 5]
    ]
