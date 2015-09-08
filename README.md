# hs-numerical

Numerical algorithms in pure Haskell.

The algorithms are based on *Numerical Recipes in C*. The abstractions aren't.

## Example

```haskell
> ridderToRoot sin (3.1, 3.2) 1e-10
Just 3.141592653589793
```

## Motivation

I translated a few of the algorithms in NR out of curiosity, focusing on
readability. I wanted to learn more about the algorithms and see how Haskell
and C differ.

The main abstraction is the Refinable typeclass which provides a way to capture
the common structure of numerical algorithms. It reduces boiler plate a lot and
makes it easy to identify the unique parts of different algorithms.
