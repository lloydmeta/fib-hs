# Fib HS [![Build Status](https://travis-ci.org/lloydmeta/fib-hs.svg?branch=master)](https://travis-ci.org/lloydmeta/fib-hs)

Just trying out Haskell [Stack](https://docs.haskellstack.org/en/stable/GUIDE/)

Includes the following:

- Different Fibonacci implementations
- Implementing read from stdin and calling a function
- Tests via HSpec
- Travis integration
- Benchmarking using criterion

# Running

`$ stack exec fib-hs-exe`

# Tests

`$ stack test`

# Benchmarks

`$ stack bench`

## Questions

1. Why is `fibLinear`, which is linear and tail-recursive, slower than `fibClassic` ??

    Fib 30 takes 498.8 ns for `fibLinear` vs 79.47 ns for `fibClassic`
    
## Full results

```
fib-hs-0.1.0.0: benchmarks
Running 1 benchmarks...
Benchmark fib-hs-bench: RUNNING...
benchmarking fibNaive/10
time                 924.5 ns   (911.9 ns .. 937.2 ns)
                     0.998 R²   (0.997 R² .. 0.999 R²)
mean                 931.7 ns   (918.9 ns .. 948.0 ns)
std dev              51.29 ns   (40.62 ns .. 65.58 ns)
variance introduced by outliers: 71% (severely inflated)
             
benchmarking fibNaive/20
time                 113.9 μs   (111.4 μs .. 116.6 μs)
                     0.996 R²   (0.995 R² .. 0.997 R²)
mean                 114.1 μs   (111.7 μs .. 116.3 μs)
std dev              7.582 μs   (6.351 μs .. 9.315 μs)
variance introduced by outliers: 65% (severely inflated)
             
benchmarking fibNaive/30
time                 13.32 ms   (12.97 ms .. 13.61 ms)
                     0.997 R²   (0.994 R² .. 0.999 R²)
mean                 14.08 ms   (13.83 ms .. 14.48 ms)
std dev              780.9 μs   (564.5 μs .. 1.245 ms)
variance introduced by outliers: 26% (moderately inflated)
             
benchmarking fibLinear/10
time                 158.4 ns   (156.1 ns .. 160.9 ns)
                     0.998 R²   (0.997 R² .. 0.999 R²)
mean                 158.1 ns   (155.5 ns .. 160.7 ns)
std dev              8.389 ns   (6.681 ns .. 10.55 ns)
variance introduced by outliers: 72% (severely inflated)
             
benchmarking fibLinear/20
time                 334.1 ns   (329.5 ns .. 338.2 ns)
                     0.998 R²   (0.998 R² .. 0.999 R²)
mean                 331.4 ns   (325.7 ns .. 337.7 ns)
std dev              19.34 ns   (15.23 ns .. 25.49 ns)
variance introduced by outliers: 75% (severely inflated)
             
benchmarking fibLinear/30
time                 505.6 ns   (494.9 ns .. 514.9 ns)
                     0.997 R²   (0.995 R² .. 0.998 R²)
mean                 498.8 ns   (490.6 ns .. 508.2 ns)
std dev              30.80 ns   (25.10 ns .. 38.84 ns)
variance introduced by outliers: 76% (severely inflated)
             
benchmarking fibClassic/10
time                 26.14 ns   (25.79 ns .. 26.51 ns)
                     0.998 R²   (0.997 R² .. 0.999 R²)
mean                 26.21 ns   (25.77 ns .. 26.62 ns)
std dev              1.445 ns   (1.224 ns .. 1.896 ns)
variance introduced by outliers: 77% (severely inflated)
             
benchmarking fibClassic/20
time                 60.15 ns   (59.20 ns .. 61.01 ns)
                     0.998 R²   (0.997 R² .. 0.999 R²)
mean                 60.34 ns   (59.25 ns .. 61.40 ns)
std dev              3.558 ns   (2.948 ns .. 4.310 ns)
variance introduced by outliers: 78% (severely inflated)
             
benchmarking fibClassic/30
time                 80.14 ns   (78.88 ns .. 81.30 ns)
                     0.998 R²   (0.997 R² .. 0.999 R²)
mean                 79.47 ns   (78.18 ns .. 80.60 ns)
std dev              4.070 ns   (3.260 ns .. 4.862 ns)
variance introduced by outliers: 72% (severely inflated)
             
Benchmark fib-hs-bench: FINISH
```