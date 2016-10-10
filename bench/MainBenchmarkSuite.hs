import Criterion.Main
import Fibonacci

-- Our benchmark harness.
main :: IO ()
main = defaultMain [
  bgroup "fibNaive" $ benchesFor fibNaive,
  bgroup "fibLinear" $ benchesFor fibLinear,
  bgroup "fibClassic" $ benchesFor fibClassic
  ]

benchesFor :: (Int -> Int) -> [Benchmark]
benchesFor f = map (\to -> bench (show to) $ nf f to) [10, 20, 30]