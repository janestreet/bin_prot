#!/bin/sh
set -e -o pipefail
flags="$(pkg-config --static mirage-xen --cflags)" || flags=""
echo "($flags)"
