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
    
### Answer

`Let-floating`, which is enabled by default because `full-laziness` is on by default in GHC,
causes the `fib'` local variable in `fibClassic` to get pulled out into something like a top-level constant.

Disabling `let-floating` by using the `-fno-full-laziness` flag turns the tables.

This was surprising (and a questionable default) to me because it could potentially have
a disastrous effect on run-time memory usage (running fib to 100 million would result in holding a huge
list in memory forever).
    
## Full results

With default GHC options

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

With `-fno-full-laziness` GHC option, which [disables let-floating](https://downloads.haskell.org/~ghc/7.10.1/docs/html/users_guide/options-optimise.html).
For more flags, see [this list](https://downloads.haskell.org/~ghc/7.10.1/docs/html/users_guide/options-optimise.html). 

```
fib-hs-0.1.0.0: benchmarks
Running 1 benchmarks...
Benchmark fib-hs-bench: RUNNING...
benchmarking fibNaive/10
time                 1.011 μs   (991.5 ns .. 1.030 μs)
                     0.997 R²   (0.996 R² .. 0.998 R²)
mean                 1.006 μs   (988.4 ns .. 1.026 μs)
std dev              66.08 ns   (52.75 ns .. 84.40 ns)
variance introduced by outliers: 78% (severely inflated)
             
benchmarking fibNaive/20
time                 121.4 μs   (119.5 μs .. 123.6 μs)
                     0.996 R²   (0.994 R² .. 0.998 R²)
mean                 124.2 μs   (121.9 μs .. 126.8 μs)
std dev              8.340 μs   (6.625 μs .. 11.12 μs)
variance introduced by outliers: 66% (severely inflated)
             
benchmarking fibNaive/30
time                 15.09 ms   (14.56 ms .. 15.54 ms)
                     0.995 R²   (0.991 R² .. 0.998 R²)
mean                 15.58 ms   (15.31 ms .. 15.85 ms)
std dev              712.8 μs   (559.1 μs .. 917.2 μs)
variance introduced by outliers: 16% (moderately inflated)
             
benchmarking fibLinear/10
time                 110.6 ns   (108.8 ns .. 112.3 ns)
                     0.997 R²   (0.996 R² .. 0.998 R²)
mean                 110.2 ns   (108.2 ns .. 112.3 ns)
std dev              6.664 ns   (5.516 ns .. 8.040 ns)
variance introduced by outliers: 78% (severely inflated)
             
benchmarking fibLinear/20
time                 260.8 ns   (257.4 ns .. 263.7 ns)
                     0.998 R²   (0.998 R² .. 0.999 R²)
mean                 254.9 ns   (251.4 ns .. 258.2 ns)
std dev              11.97 ns   (10.70 ns .. 13.56 ns)
variance introduced by outliers: 66% (severely inflated)
             
benchmarking fibLinear/30
time                 428.5 ns   (425.6 ns .. 431.7 ns)
                     0.998 R²   (0.997 R² .. 0.999 R²)
mean                 431.4 ns   (424.4 ns .. 439.1 ns)
std dev              24.65 ns   (19.92 ns .. 31.54 ns)
variance introduced by outliers: 74% (severely inflated)
             
benchmarking fibLinearBang/10
time                 82.07 ns   (80.22 ns .. 83.94 ns)
                     0.997 R²   (0.996 R² .. 0.998 R²)
mean                 80.77 ns   (79.33 ns .. 82.01 ns)
std dev              4.414 ns   (3.734 ns .. 5.267 ns)
variance introduced by outliers: 75% (severely inflated)
             
benchmarking fibLinearBang/20
time                 180.3 ns   (177.5 ns .. 182.9 ns)
                     0.998 R²   (0.997 R² .. 0.999 R²)
mean                 177.8 ns   (174.5 ns .. 180.7 ns)
std dev              9.882 ns   (8.357 ns .. 12.08 ns)
variance introduced by outliers: 74% (severely inflated)
             
benchmarking fibLinearBang/30
time                 256.8 ns   (250.9 ns .. 263.8 ns)
                     0.996 R²   (0.995 R² .. 0.998 R²)
mean                 259.8 ns   (255.7 ns .. 263.2 ns)
std dev              12.95 ns   (10.76 ns .. 15.42 ns)
variance introduced by outliers: 69% (severely inflated)
             
benchmarking fibClassic/10
time                 238.1 ns   (234.5 ns .. 242.5 ns)
                     0.997 R²   (0.995 R² .. 0.998 R²)
mean                 243.5 ns   (239.4 ns .. 248.6 ns)
std dev              15.32 ns   (12.51 ns .. 18.82 ns)
variance introduced by outliers: 78% (severely inflated)
             
benchmarking fibClassic/20
time                 474.3 ns   (469.0 ns .. 479.8 ns)
                     0.999 R²   (0.998 R² .. 0.999 R²)
mean                 474.0 ns   (470.5 ns .. 480.4 ns)
std dev              15.57 ns   (9.727 ns .. 23.87 ns)
variance introduced by outliers: 47% (moderately inflated)
             
benchmarking fibClassic/30
time                 731.2 ns   (720.0 ns .. 742.9 ns)
                     0.998 R²   (0.997 R² .. 0.999 R²)
mean                 738.4 ns   (727.3 ns .. 749.5 ns)
std dev              40.12 ns   (33.71 ns .. 52.35 ns)
variance introduced by outliers: 71% (severely inflated)
             
Benchmark fib-hs-bench: FINISH
```