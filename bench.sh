#!/usr/bin/env bash
#
# Evaluate which function is fastest on this machine right now, and

source "$(dirname $0)"/preamble.sh

for i in $IMPLEMENTATIONS; do
    "$CC" -DSTANDALONE_FOR_PERF $CFLAGS -o $BUILD/$i.perf $SRC/$i.c $SRC/stub.c $LDFLAGS
done

for i in $IMPLEMENTATIONS; do
    perf stat -r 10 $BUILD/$i.perf
done
