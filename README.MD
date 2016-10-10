# Fib HS

Just trying out Haskell [Stack](https://docs.haskellstack.org/en/stable/GUIDE/)

- Implementing read from stdin and calling a function
- Tests via HSpec
- Benchmarking using criterion

# Running

`$ stack exec fib-hs-exec`

# Tests

`$ stack test`

# Benchmarks

`$ stack bench`

## Questions

1. Why is `fibLinear` slower than `fibClassic` ??
    Fib 11 takes 67.59 ns for `fibLinear` vs 28.18 ns for `fibClassic`