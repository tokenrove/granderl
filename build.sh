#!/usr/bin/env bash
#
# This is just a shell script rather than a Makefile since there's not
# much point avoiding rebuilds here.

source "$(dirname $0)"/preamble.sh

TARGET=${TARGET:-./priv/granderl.so}
SRC=./c_src
IMPLEMENTATION=${IMPLEMENTATION:-pcg32}

mkdir -p priv

up_to_date_p() {
    for i in $SRC/*.c; do
        if [ "$TARGET" -ot "$i" ]; then exit 1; fi
    done
    exit 0
}

if (up_to_date_p); then exit 0; fi

exec "$CC" $CFLAGS -shared -o "$TARGET" $SRC/$IMPLEMENTATION.c $SRC/granderl.c $LDFLAGS
