set -eu -o pipefail

ERTS_INCLUDE_DIR=${ERTS_INCLUDE_DIR:-$(erl -noshell -s init stop -eval "io:format(\"~s/erts-~s/include/\", [code:root_dir(), erlang:system_info(version)]).")}
CC=${CC:-cc}
CFLAGS=${CFLAGS:--fPIC -I${ERTS_INCLUDE_DIR} \
  -std=gnu11 -O3 -march=native -mtune=native \
  -ggdb -Wall -Wextra -Wno-missing-field-initializers}
LDFLAGS=${LDFLAGS:-}
if [ "$(uname -s)" = Darwin ]; then
    LDFLAGS="$LDFLAGS -flat_namespace -undefined suppress"
fi

SRC=c_src
BUILD=build
IMPLEMENTATIONS="rdrand xorshift pcg32"
