#!/bin/sh

set -e

if [ $# -lt 2 ]; then
    echo "Usage: detect-arch.sh OCAMLC CONFIG_H" >&2
    exit 2
fi

OCAMLC="$1"
CONFIG_H="$2"

SRC=test.c
trap "rm -f $OUT" EXIT

if $OCAMLC -ccopt -E -c $SRC | grep -q ARCH_SIXTYFOUR_IS_DEFINED; then
    def="#define JSC_ARCH_SIXTYFOUR"
else
    def="/* #define JSC_ARCH_SIXTYFOUR */"
fi

sentinel="CORE_`basename "$CONFIG_H" | tr a-z. A-Z_`"
cat > "$CONFIG_H"  <<EOF
#ifndef $sentinel
#define $sentinel
$def
#endif
EOF
