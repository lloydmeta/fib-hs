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

    Fib 30 takes 475.1 ns for `fibLinear` vs 78.46 ns for `fibClassic`
    
2. Why is `fibLinearBang`, which is linear and tail-recursive, slower than `fibClassic` ??

    Fib 30 takes 279.5 ns for `fibLinearBang` vs 78.46 ns for `fibClassic`
    
## Full results

```
fib-hs-0.1.0.0: benchmarks
Running 1 benchmarks...
Benchmark fib-hs-bench: RUNNING...
benchmarking fibNaive/10
time                 915.0 ns   (903.4 ns .. 927.9 ns)
                     0.998 R²   (0.997 R² .. 0.999 R²)
mean                 931.9 ns   (918.1 ns .. 947.8 ns)
std dev              48.95 ns   (40.02 ns .. 61.29 ns)
variance introduced by outliers: 69% (severely inflated)
             
benchmarking fibNaive/20
time                 114.3 μs   (112.0 μs .. 116.1 μs)
                     0.997 R²   (0.996 R² .. 0.998 R²)
mean                 112.5 μs   (110.7 μs .. 114.2 μs)
std dev              5.882 μs   (4.796 μs .. 7.396 μs)
variance introduced by outliers: 54% (severely inflated)
             
benchmarking fibNaive/30
time                 14.19 ms   (13.88 ms .. 14.55 ms)
                     0.997 R²   (0.995 R² .. 0.999 R²)
mean                 14.23 ms   (14.01 ms .. 14.44 ms)
std dev              554.4 μs   (432.9 μs .. 751.8 μs)
variance introduced by outliers: 15% (moderately inflated)
             
benchmarking fibLinear/10
time                 124.1 ns   (121.9 ns .. 126.4 ns)
                     0.998 R²   (0.996 R² .. 0.998 R²)
mean                 124.0 ns   (121.8 ns .. 126.3 ns)
std dev              7.517 ns   (6.068 ns .. 9.305 ns)
variance introduced by outliers: 78% (severely inflated)
             
benchmarking fibLinear/20
time                 281.0 ns   (276.5 ns .. 285.7 ns)
                     0.998 R²   (0.997 R² .. 0.998 R²)
mean                 282.6 ns   (277.6 ns .. 288.0 ns)
std dev              16.78 ns   (13.39 ns .. 21.98 ns)
variance introduced by outliers: 76% (severely inflated)
             
benchmarking fibLinear/30
time                 466.4 ns   (457.7 ns .. 475.3 ns)
                     0.997 R²   (0.996 R² .. 0.998 R²)
mean                 475.1 ns   (465.9 ns .. 484.2 ns)
std dev              30.95 ns   (25.34 ns .. 39.17 ns)
variance introduced by outliers: 78% (severely inflated)
             
benchmarking fibLinearBang/10
time                 86.25 ns   (84.90 ns .. 87.41 ns)
                     0.998 R²   (0.997 R² .. 0.999 R²)
mean                 85.64 ns   (84.29 ns .. 86.91 ns)
std dev              4.444 ns   (3.521 ns .. 5.833 ns)
variance introduced by outliers: 72% (severely inflated)
             
benchmarking fibLinearBang/20
time                 187.9 ns   (185.4 ns .. 190.8 ns)
                     0.997 R²   (0.996 R² .. 0.998 R²)
mean                 189.1 ns   (185.4 ns .. 193.3 ns)
std dev              13.18 ns   (10.75 ns .. 15.99 ns)
variance introduced by outliers: 82% (severely inflated)
             
benchmarking fibLinearBang/30
time                 280.7 ns   (276.7 ns .. 284.7 ns)
                     0.998 R²   (0.997 R² .. 0.999 R²)
mean                 279.5 ns   (275.0 ns .. 284.4 ns)
std dev              15.69 ns   (12.60 ns .. 19.62 ns)
variance introduced by outliers: 74% (severely inflated)
             
benchmarking fibClassic/10
time                 29.74 ns   (29.25 ns .. 30.37 ns)
                     0.997 R²   (0.996 R² .. 0.998 R²)
mean                 29.93 ns   (29.43 ns .. 30.54 ns)
std dev              1.877 ns   (1.578 ns .. 2.376 ns)
variance introduced by outliers: 81% (severely inflated)
             
benchmarking fibClassic/20
time                 61.50 ns   (60.39 ns .. 62.53 ns)
                     0.997 R²   (0.996 R² .. 0.999 R²)
mean                 61.74 ns   (60.71 ns .. 62.84 ns)
std dev              3.594 ns   (2.995 ns .. 4.470 ns)
variance introduced by outliers: 77% (severely inflated)
             
benchmarking fibClassic/30
time                 78.00 ns   (76.82 ns .. 79.06 ns)
                     0.998 R²   (0.997 R² .. 0.999 R²)
mean                 78.46 ns   (77.16 ns .. 79.95 ns)
std dev              4.861 ns   (3.899 ns .. 6.115 ns)
variance introduced by outliers: 79% (severely inflated)
             
Benchmark fib-hs-bench: FINISH
```