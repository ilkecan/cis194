import Exercise4
  ( sundaram1,
    sundaram2,
  )
import Test.Tasty.Bench

main :: IO ()
main =
  Test.Tasty.Bench.defaultMain
    [ bench "sundaram1" $ nf sundaram1 1000,
      bench "sundaram2" $ nf sundaram2 1000
    ]
