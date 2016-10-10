{-# LANGUAGE BangPatterns #-}

module Fibonacci(
    fibNaive,
    fibLinear,
    fibClassic,
    fibLinearBang
) where

-- textbook
fibNaive :: Int -> Integer
fibNaive 0 = 0
fibNaive 1 = 1
fibNaive x = fibNaive(x-1) + fibNaive(x-2)

-- Linear
fibLinear :: Int -> Integer
fibLinear 0 = 0
fibLinear 1 = 1
fibLinear x = fibLin' 2 1 2
  where
      fibLin' i y z | i == x = y
      fibLin' i y z          = fibLin' (i+1) z (z+y)

-- Linear banged
fibLinearBang :: Int -> Integer
fibLinearBang 0 = 0
fibLinearBang 1 = 1
fibLinearBang x = fibLin' 2 1 2
  where
      fibLin' (!i) (!y) (!z) | i == x = y
      fibLin' (!i) (!y) (!z)          = fibLin' (i+1) z (z+y)

-- Haskeller classic lazy
fibClassic :: Int -> Integer
fibClassic n = fib' !! n
    where fib' = 0:1:zipWith (+) fib' (tail fib')
