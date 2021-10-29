import Exercise4
  ( sieveSundaram1,
    sieveSundaram2,
  )
import Test.Tasty.Bench

main :: IO ()
main =
  Test.Tasty.Bench.defaultMain
    [ bench "sieveSundaram1" $ nf sieveSundaram1 1000,
      bench "sieveSundaram2" $ nf sieveSundaram2 1000
    ]
