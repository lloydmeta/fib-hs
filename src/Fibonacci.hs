module Fibonacci(
    fibNaive,
    fibLinear,
    fibClassic
) where

-- textbook
fibNaive :: Int -> Integer
fibNaive 0 = 0
fibNaive 1 = 1
fibNaive x = fibNaive(x-1) + fibNaive(x-2)

-- Linear
fibLinear :: Int -> Integer
fibLinear x = fst $ fibLin' 0 0 1
  where
      fibLin' _ _ _ | x == 0 = (0, 0)
      fibLin' _ _ _ | x == 1 = (1, 0)
      fibLin' i y z | i == x = (y, z)
      fibLin' i y z = fibLin' (i+1) z (z+y)

-- Haskeller classic lazy
fibClassic :: Int -> Integer
fibClassic n = fib' !! n
    where fib' = 0:1:zipWith (+) fib' (tail fib')
