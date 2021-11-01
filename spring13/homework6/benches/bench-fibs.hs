import Exercise1 (fibs1)
import Exercise2 (fibs2)
import Test.Tasty.Bench

main :: IO ()
main =
  Test.Tasty.Bench.defaultMain
    [ -- how to benchmark these functions correctly? CPU time of both functions
      -- are shown as similar, but they shouldn't be
      bench "fibs1" $ nf (take 35) fibs1,
      bench "fibs2" $ nf (take 35) fibs2
    ]
