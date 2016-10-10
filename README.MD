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

    Fib 30 takes 361.8 ns for `fibLinear` vs 79.96 ns for `fibClassic`
    
## Full results

```
fib-hs-0.1.0.0: benchmarks
Running 1 benchmarks...
Benchmark fib-hs-bench: RUNNING...
benchmarking fibNaive/10
time                 474.7 ns   (464.7 ns .. 483.7 ns)
                     0.997 R²   (0.995 R² .. 0.998 R²)
mean                 474.7 ns   (465.2 ns .. 483.3 ns)
std dev              30.13 ns   (25.15 ns .. 35.03 ns)
variance introduced by outliers: 77% (severely inflated)

benchmarking fibNaive/20
time                 58.41 μs   (57.72 μs .. 59.17 μs)
                     0.998 R²   (0.996 R² .. 0.999 R²)
mean                 59.28 μs   (58.44 μs .. 60.48 μs)
std dev              3.409 μs   (2.714 μs .. 4.506 μs)
variance introduced by outliers: 61% (severely inflated)

benchmarking fibNaive/30
time                 7.253 ms   (7.104 ms .. 7.395 ms)
                     0.998 R²   (0.996 R² .. 0.999 R²)
mean                 7.321 ms   (7.224 ms .. 7.423 ms)
std dev              284.0 μs   (222.2 μs .. 399.3 μs)
variance introduced by outliers: 16% (moderately inflated)

benchmarking fibLinear/10
time                 120.9 ns   (118.6 ns .. 123.4 ns)
                     0.997 R²   (0.996 R² .. 0.999 R²)
mean                 124.0 ns   (121.9 ns .. 126.2 ns)
std dev              7.017 ns   (5.538 ns .. 9.005 ns)
variance introduced by outliers: 75% (severely inflated)

benchmarking fibLinear/20
time                 244.0 ns   (240.7 ns .. 247.5 ns)
                     0.997 R²   (0.996 R² .. 0.999 R²)
mean                 246.9 ns   (241.7 ns .. 252.3 ns)
std dev              17.93 ns   (14.65 ns .. 22.32 ns)
variance introduced by outliers: 83% (severely inflated)

benchmarking fibLinear/30
time                 357.9 ns   (352.9 ns .. 362.4 ns)
                     0.998 R²   (0.997 R² .. 0.999 R²)
mean                 361.8 ns   (355.6 ns .. 368.8 ns)
std dev              22.84 ns   (18.88 ns .. 28.50 ns)
variance introduced by outliers: 78% (severely inflated)

benchmarking fibClassic/10
time                 30.18 ns   (29.70 ns .. 30.62 ns)
                     0.998 R²   (0.997 R² .. 0.999 R²)
mean                 30.28 ns   (29.79 ns .. 30.79 ns)
std dev              1.666 ns   (1.352 ns .. 2.051 ns)
variance introduced by outliers: 76% (severely inflated)

benchmarking fibClassic/20
time                 60.84 ns   (60.04 ns .. 61.62 ns)
                     0.998 R²   (0.996 R² .. 0.999 R²)
mean                 60.70 ns   (59.58 ns .. 61.76 ns)
std dev              3.669 ns   (3.175 ns .. 4.348 ns)
variance introduced by outliers: 79% (severely inflated)

benchmarking fibClassic/30
time                 78.89 ns   (78.02 ns .. 79.98 ns)
                     0.998 R²   (0.997 R² .. 0.999 R²)
mean                 79.96 ns   (78.72 ns .. 81.39 ns)
std dev              4.485 ns   (3.596 ns .. 6.385 ns)
variance introduced by outliers: 76% (severely inflated)
```