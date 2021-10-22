import Exercise6
  ( idealK1,
    idealK2,
  )
import Test.Tasty.Bench

main :: IO ()
main =
  Test.Tasty.Bench.defaultMain
    [ bench "idealK1" $ nf idealK1 12345,
      bench "idealK2" $ nf idealK2 12345
    ]
