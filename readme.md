# `monads-tf`

Monad classes using type families, with instances for various monad transformers,
inspired by the paper
[Functional Programming with Overloading and Higher-Order Polymorphism][paper],
by Mark P Jones, in _Advanced School of Functional Programming_, 1995.

This repository hosts both the package code and some demo examples.
See the [package readme](./monads-tf/readme.md) for more information.

## Building

To build and test with all supported compiler versions:

```shell
nix build .#testConfigurations.all --no-link
```

[paper]: https://web.cecs.pdx.edu/~mpj/pubs/springschool.html
