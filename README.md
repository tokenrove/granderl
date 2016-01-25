Experiments in fast "random" number generation for Erlang; though they
have the same interface as `rand:uniform/1`, think of them more like
`phash2/2` with a timestamp -- not really random, but fast
(hopefully).

