#!/usr/bin/env bash
#
# Evaluate which function is fastest on this machine right now, and

source "$(dirname $0)"/preamble.sh

for i in $IMPLEMENTATIONS; do
    "$CC" -DSTANDALONE_FOR_DIEHARDER $CFLAGS -o $BUILD/$i.dieharder $SRC/$i.c $SRC/stub.c $LDFLAGS
done

DIEHARD_TESTS="diehard_birthdays diehard_bitstream diehard_opso diehard_oqso \
  diehard_runs sts_monobit sts_runs rgb_lagged_sum"
DIEHARD_INFO='-D test_name -D assessment -D rate'

for imp in $IMPLEMENTATIONS; do
    echo $imp
    for test in $DIEHARD_TESTS; do
        $BUILD/$imp.dieharder | dieharder -g 200 -d $test $DIEHARD_INFO
    done
done
