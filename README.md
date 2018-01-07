# lbiz - C++11 constexpr bigint/bigfixed library

lbiz library contains following:

* BigInt with carry save adder and Fast Modulo Transform (FMT) multiplier
* BigFixed with reciprocal/inverse square root calculation by Newton's method

## Example

Calculate pi more than 100 digits using Gauss-Legendre algorithm at compile-time

```
clang++ -std=c++11 -fconstexpr-steps=4967573 main.cpp
```

