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

    Fib 30 takes 443.0 ns for `fibLinear` vs 73.51 ns for `fibClassic`
    
## Full results

```
fib-hs-0.1.0.0: benchmarks
Running 1 benchmarks...
Benchmark fib-hs-bench: RUNNING...
benchmarking fibNaive/10
time                 882.4 ns   (868.3 ns .. 896.6 ns)
                     0.998 R²   (0.997 R² .. 0.999 R²)
mean                 895.4 ns   (880.1 ns .. 912.1 ns)
std dev              52.31 ns   (43.74 ns .. 64.55 ns)
variance introduced by outliers: 73% (severely inflated)
             
benchmarking fibNaive/20
time                 111.1 μs   (108.0 μs .. 115.5 μs)
                     0.994 R²   (0.990 R² .. 0.998 R²)
mean                 109.2 μs   (107.4 μs .. 111.3 μs)
std dev              6.692 μs   (5.465 μs .. 8.795 μs)
variance introduced by outliers: 62% (severely inflated)
             
benchmarking fibNaive/30
time                 13.95 ms   (13.66 ms .. 14.19 ms)
                     0.997 R²   (0.995 R² .. 0.999 R²)
mean                 14.56 ms   (14.27 ms .. 14.91 ms)
std dev              776.7 μs   (603.0 μs .. 1.039 ms)
variance introduced by outliers: 23% (moderately inflated)
             
benchmarking fibLinear/10
time                 112.9 ns   (110.7 ns .. 115.6 ns)
                     0.997 R²   (0.996 R² .. 0.998 R²)
mean                 113.7 ns   (111.9 ns .. 115.6 ns)
std dev              6.166 ns   (5.138 ns .. 8.411 ns)
variance introduced by outliers: 74% (severely inflated)
             
benchmarking fibLinear/20
time                 263.9 ns   (259.0 ns .. 269.0 ns)
                     0.998 R²   (0.997 R² .. 0.998 R²)
mean                 265.7 ns   (260.9 ns .. 270.4 ns)
std dev              15.24 ns   (12.83 ns .. 18.93 ns)
variance introduced by outliers: 74% (severely inflated)
             
benchmarking fibLinear/30
time                 455.4 ns   (446.2 ns .. 462.3 ns)
                     0.997 R²   (0.996 R² .. 0.998 R²)
mean                 443.0 ns   (435.3 ns .. 451.1 ns)
std dev              26.36 ns   (22.98 ns .. 30.33 ns)
variance introduced by outliers: 75% (severely inflated)
             
benchmarking fibClassic/10
time                 29.50 ns   (28.76 ns .. 30.02 ns)
                     0.997 R²   (0.995 R² .. 0.998 R²)
mean                 29.84 ns   (29.38 ns .. 30.41 ns)
std dev              1.615 ns   (1.318 ns .. 2.011 ns)
variance introduced by outliers: 76% (severely inflated)
             
benchmarking fibClassic/20
time                 59.80 ns   (58.91 ns .. 60.75 ns)
                     0.998 R²   (0.997 R² .. 0.999 R²)
mean                 60.47 ns   (59.39 ns .. 61.40 ns)
std dev              3.265 ns   (2.673 ns .. 4.173 ns)
variance introduced by outliers: 75% (severely inflated)
             
benchmarking fibClassic/30
time                 73.16 ns   (71.85 ns .. 74.48 ns)
                     0.997 R²   (0.996 R² .. 0.998 R²)
mean                 73.51 ns   (72.38 ns .. 74.82 ns)
std dev              4.045 ns   (3.420 ns .. 5.226 ns)
variance introduced by outliers: 75% (severely inflated)
             
Benchmark fib-hs-bench: FINISH
```