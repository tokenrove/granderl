#!/usr/bin/env bash
#
# Evaluate which function is fastest on this machine right now, and

source "$(dirname $0)"/preamble.sh

for i in $IMPLEMENTATIONS; do
    "$CC" -DSTANDALONE_FOR_DIEHARDER $CFLAGS -o $BUILD/$i.dieharder $SRC/$i.c $SRC/stub.c $LDFLAGS
done

for i in $IMPLEMENTATIONS; do
    echo $i
    $BUILD/$i.dieharder | dieharder -a -g 200
done
