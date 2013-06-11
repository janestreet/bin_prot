#!/bin/sh

set -e

if [ $# -lt 1 ]; then
    echo "Usage: arch.sh OCAMLC" >&2
    exit 2
fi

OCAMLC="$1"

if $OCAMLC -ccopt -E -c config/test.c | grep -q ARCH_SIXTYFOUR_IS_DEFINED; then
    arch_sixtyfour=true
else
    arch_sixtyfour=false
fi

if [ -e setup.data ]; then
    sed '/^arch_sixtyfour=/d' setup.data > setup.data.new
    mv setup.data.new setup.data
fi

cat >> setup.data <<EOF
arch_sixtyfour="$arch_sixtyfour"
EOF
