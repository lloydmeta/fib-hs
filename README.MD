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
time                 861.5 ns   (848.0 ns .. 875.2 ns)
                     0.998 R²   (0.997 R² .. 0.999 R²)
mean                 874.5 ns   (861.2 ns .. 888.5 ns)
std dev              45.66 ns   (37.83 ns .. 59.65 ns)
variance introduced by outliers: 68% (severely inflated)
             
benchmarking fibNaive/20
time                 107.8 μs   (105.1 μs .. 110.6 μs)
                     0.996 R²   (0.994 R² .. 0.998 R²)
mean                 106.9 μs   (105.1 μs .. 109.2 μs)
std dev              6.694 μs   (5.685 μs .. 7.902 μs)
variance introduced by outliers: 63% (severely inflated)
             
benchmarking fibNaive/30
time                 12.65 ms   (12.44 ms .. 12.87 ms)
                     0.998 R²   (0.996 R² .. 0.999 R²)
mean                 13.34 ms   (13.04 ms .. 13.76 ms)
std dev              935.3 μs   (680.8 μs .. 1.204 ms)
variance introduced by outliers: 35% (moderately inflated)
             
benchmarking fibLinear/10
time                 144.1 ns   (142.0 ns .. 146.1 ns)
                     0.998 R²   (0.998 R² .. 0.999 R²)
mean                 143.5 ns   (141.6 ns .. 145.5 ns)
std dev              6.660 ns   (5.889 ns .. 7.825 ns)
variance introduced by outliers: 67% (severely inflated)
             
benchmarking fibLinear/20
time                 311.0 ns   (305.2 ns .. 315.7 ns)
                     0.998 R²   (0.997 R² .. 0.999 R²)
mean                 308.4 ns   (303.9 ns .. 314.8 ns)
std dev              18.25 ns   (14.05 ns .. 26.08 ns)
variance introduced by outliers: 75% (severely inflated)
             
benchmarking fibLinear/30
time                 458.1 ns   (449.8 ns .. 466.9 ns)
                     0.997 R²   (0.996 R² .. 0.998 R²)
mean                 466.6 ns   (459.7 ns .. 476.1 ns)
std dev              27.38 ns   (20.22 ns .. 45.96 ns)
variance introduced by outliers: 75% (severely inflated)
             
benchmarking fibClassic/10
time                 24.56 ns   (24.01 ns .. 25.06 ns)
                     0.997 R²   (0.996 R² .. 0.998 R²)
mean                 24.73 ns   (24.36 ns .. 25.21 ns)
std dev              1.425 ns   (1.208 ns .. 1.721 ns)
variance introduced by outliers: 78% (severely inflated)
             
benchmarking fibClassic/20
time                 56.42 ns   (55.63 ns .. 57.40 ns)
                     0.998 R²   (0.996 R² .. 0.999 R²)
mean                 56.98 ns   (56.06 ns .. 58.25 ns)
std dev              3.646 ns   (2.878 ns .. 5.005 ns)
variance introduced by outliers: 81% (severely inflated)
             
benchmarking fibClassic/30
time                 73.93 ns   (72.79 ns .. 75.18 ns)
                     0.998 R²   (0.997 R² .. 0.999 R²)
mean                 75.28 ns   (74.05 ns .. 76.95 ns)
std dev              4.638 ns   (3.733 ns .. 7.101 ns)
variance introduced by outliers: 79% (severely inflated)
             
Benchmark fib-hs-bench: FINISH
```