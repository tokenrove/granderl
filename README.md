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

Use [rebar3](http://www.rebar3.org/).  To override the default
implementation choice, you can set the `IMPLEMENTATION` environment
variable:

```
$ IMPLEMENTATION=rdrand rebar3 compile
```

# Interface

All functions take an integer between 1 and 2<sup>32</sup>-1, and
return an integer between 1 and the supplied integer, like
`rand:uniform/1`.

## `granderl:`

### `uniform(N :: 1..4294967295) -> 1..4294967295`

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

## `msws`

Middle Square Weyl Sequence, keeping state in thread-local storage.
Initialized on first call.

# References

Fog, Agner. ["Pseudo-Random Number Generators for Vector Processors and Multicore Processors."](http://orbit.dtu.dk/ws/files/118886115/Fog_Pseudo_Random_Number_Generators.pdf) Journal of Modern Applied Statistical Methods 14.1 (2015): 308-334.

Marsaglia, George. ["Xorshift RNGs."](http://www.jstatsoft.org/article/view/v008i14) Journal of Statistical Software 8.14 (2003): 1-6.

O'Neill, M.E. ["PCG: A Family of Simple Fast Space-Efficient Statistically Good Algorithms for Random Number Generation"](http://www.pcg-random.org/pdf/toms-oneill-pcg-family-v1.02.pdf).

Widynsky, Bernard. ["Middle Square Weyl Sequence RNG"](https://arxiv.org/pdf/1704.00358.pdf).

# License

Because the pcg32 code is derived from Apache-licensed code (see
c_src/pcg32.c), this package is also
[Apache licensed](http://www.apache.org/licenses/LICENSE-2.0).
