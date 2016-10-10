import Criterion.Main
import Fibonacci

-- Our benchmark harness.
main :: IO ()
main = defaultMain [
  bgroup "fibNaive" [
                 bench "1"  $ whnf fibNaive 1
               , bench "5"  $ whnf fibNaive 5
               , bench "9"  $ whnf fibNaive 9
               , bench "11" $ whnf fibNaive 11
               ],
  bgroup "fibLinear" [
                 bench "1"  $ whnf fibLinear 1
               , bench "5"  $ whnf fibLinear 5
               , bench "9"  $ whnf fibLinear 9
               , bench "11" $ whnf fibLinear 11
               ],
  bgroup "fibClassic" [
                 bench "1"  $ whnf fibClassic 1
               , bench "5"  $ whnf fibClassic 5
               , bench "9"  $ whnf fibClassic 9
               , bench "11" $ whnf fibClassic 11
               ]
  ]