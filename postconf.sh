#!/bin/sh

export PKG_CONFIG_PATH="$(opam config var prefix)/lib/pkgconfig"

# Bridge through $XEN_CFLAGS from the environment into Oasis' world.
#
XEN_CFLAGS="disabled"
grep -q '^xen="true"' setup.data && XEN_CFLAGS="$(pkg-config --static mirage-xen --cflags)"
echo "XEN_CFLAGS=\"${XEN_CFLAGS}\"" >> setup.data
