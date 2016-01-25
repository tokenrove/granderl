# What is this?

Experiments in fast "random" number generation for Erlang; though they
have the same interface as `rand:uniform/1`, think of them more like
`phash2/2` with a timestamp -- not really random, but fast
(hopefully).

# Building

Use [rebar3](http://www.rebar3.org/).

# Interface, subject to change

All functions take an integer between 1 and 2<sup>32</sup>-1, and
return an integer between 0 and the supplied integer minus one.

## `granderl:`

### `rdrand/1`

32-bits of [`RDRAND`](https://en.wikipedia.org/wiki/RdRand).

### `xorshift_tls/1`

Marsaglia's original
[xorshift](https://en.wikipedia.org/wiki/Xorshift) (with no
multiplies), keeping state in thread-local storage.  Initialized on
first call.

### `xorshift_yolo/1`

Xorshift with global state and no attempts at synchronization.
Intended to determine the costs of other synchronization approaches.

### `rdtsc_mod/1`

[`RDTSC`](https://en.wikipedia.org/wiki/Time_Stamp_Counter) modulo
`n`.  Definitely not random, but unpredictable enough for some
purposes.

### `xorshift_rdtsc/1`

Xorshift seeded every time from
`RDTSC`, for comparison with `rdtsc_mod/1`.
