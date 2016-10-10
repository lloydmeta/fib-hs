import Criterion.Main
import Fibonacci

-- Our benchmark harness.
main :: IO ()
main = defaultMain [
  bgroup "fibNaive" $ benchesFor fibNaive,
  bgroup "fibLinear" $ benchesFor fibLinear,
  bgroup "fibLinearBang" $ benchesFor fibLinearBang,
  bgroup "fibClassic" $ benchesFor fibClassic
  ]

benchesFor :: (Int -> Integer) -> [Benchmark]
benchesFor f = map (\to -> bench (show to) $ nf f to) [10, 20, 30]