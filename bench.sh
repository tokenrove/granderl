#!/usr/bin/env bash
#
# Eventually this will evaluate which function is fastest on this
# machine for the purpose of choosing an implementation; presently
# this just runs a very basic benchmark for crude evaluation.

source "$(dirname $0)"/preamble.sh

for i in $IMPLEMENTATIONS; do
    "$CC" -DSTANDALONE_FOR_PERF $CFLAGS -o $BUILD/$i.perf $SRC/$i.c $SRC/stub.c $LDFLAGS
done

for i in $IMPLEMENTATIONS; do
    perf stat -r 10 $BUILD/$i.perf
done
