#!/bin/sh

# Create a sysroot for a separate machine through SSH.
#
# Rerunning this script on a directory already containing a sysroot will
# automatically update it.

if [ $# -lt 1 ]; then
    echo "USAGE: create_sysroot.sh [USER@]<HOST> [OUT_DIR]"
    exit 1
fi

# Since we're copying system files, require root access.
if [ "$EUID" -ne 0 ]; then
    echo "Must be run as root"
    exit 2
fi

REMOTE="$1"
SYSROOT="$2"

# Use ./sysroot by default.
if [ -z "$SYSROOT" ]; then
    SYSROOT="./sysroot"
fi
echo "Creating sysroot at '$SYSROOT'…"

# Create the sysroot.
mkdir -p "$SYSROOT"
rsync -Phav "$REMOTE":/{lib,lib64,usr} "$SYSROOT" --exclude "/usr/bin" --exclude "/usr/sbin"

# Create lib64 symlinks for compatibility.
if [ ! -d "${SYSROOT}/lib64" ]; then
    ln -s "./lib" "${SYSROOT}/lib64"
fi

if [ ! -d "${SYSROOT}/usr/lib64" ]; then
    ln -s "./lib" "${SYSROOT}/usr/lib64"
fi
