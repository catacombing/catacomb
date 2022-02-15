#!/bin/sh

# Run project on a separate device through SSH.

if [ "$#" -eq 0 ]; then
    echo "USAGE: run.sh [USER@]<HOST>"
    exit
fi

remote="$1"

# Exit on error
set -e

# Build project
scriptdir="$(dirname ${BASH_SOURCE[0]})"
"$scriptdir/build.sh"

# Find executable
binpath=$(find ./target/aarch64-unknown-linux-gnu/debug -maxdepth 1 -type f -executable | head -n 1)
bin=$(basename "$binpath")

# Strip to improve copy speed
aarch64-linux-gnu-strip "$binpath"

# Kill potentially running previous executables
ssh "$remote" "pkill $bin; rm $bin"

# Copy to the remote machine
scp "$binpath" "${remote}:~"

# Execute on TTY1
ssh "$remote" "setsid sh -c './$bin <> /dev/tty1'"
