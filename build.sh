#!/bin/sh

# Build project for aarch64.

# Target platform sysroot (default: ./sysroot).
SYSROOT="$1"
if [ -z "$SYSROOT" ]; then
    SYSROOT="./sysroot"
fi
SYSROOT=$(realpath "$SYSROOT")

# Setup pkgconfig.
export PKG_CONFIG_LIBDIR="${SYSROOT}/usr/lib/pkgconfig:${SYSROOT}/usr/share/pkgconfig"
export PKG_CONFIG_SYSROOT_DIR="${SYSROOT}"
export PKG_CONFIG_ALLOW_CROSS=true

# Setup linker.
export RUSTFLAGS="-C linker=aarch64-linux-gnu-gcc -C link-arg=--sysroot=${SYSROOT}"

# Build!
cargo build --target=aarch64-unknown-linux-gnu
