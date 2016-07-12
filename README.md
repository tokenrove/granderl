# What is this?

Experiments in fast "random" number generation for Erlang; though they
have the same interface as `rand:uniform/1`, think of them more like
`phash2/2` with a timestamp -- not really random, but fast
(hopefully).

Specifically, some of the PRNGs here use methods that are known to be
biased.  They are not suitably for any application which assumes
uniformity.  They are also not guaranteed to produce uncorrelated
streams when used in parallel.

# Building

Use [rebar3](http://www.rebar3.org/).

# Interface

All functions take an integer between 1 and 2<sup>32</sup>-1, and
return an integer between 0 and the supplied integer minus one.

## `granderl:`

### `uniform/1`


# Implementations

## `rdrand`

32-bits of [`RDRAND`](https://en.wikipedia.org/wiki/RdRand).

## `xorshift`

Marsaglia's original
[xorshift](https://en.wikipedia.org/wiki/Xorshift) (with no
multiplies), keeping state in thread-local storage.  Initialized on
first call.

## `pcg32`

[PCG](http://www.pcg-random.org), TLS.
