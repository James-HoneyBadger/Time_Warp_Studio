#!/usr/bin/env bash
# Build the Studio if it is missing or out of date, then run it.
#
#   ./run.sh                         an empty workspace
#   ./run.sh Examples/logo/mandala.logo
#   ./run.sh --run Examples/basic/hello.bas
#
# TIME_WARP_SKIP_BUILD=1 skips the incremental build (useful when the tree is
# known good); TIME_WARP_BUILD_DIR points at another build tree.
set -euo pipefail

root="$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" && pwd)"
build_dir="${TIME_WARP_BUILD_DIR:-$root/native/build}"
# An override is relative to the checkout, not to wherever the caller
# happened to be: `./run.sh` and a script in another directory should
# build in the same place.
if [[ "$build_dir" != /* ]]; then
    build_dir="$root/$build_dir"
fi
binary="$build_dir/time_warp_ide"

jobs() {
    if command -v nproc >/dev/null 2>&1; then
        nproc
    elif command -v sysctl >/dev/null 2>&1; then
        sysctl -n hw.ncpu
    else
        printf '4\n'
    fi
}

if [[ "${TIME_WARP_SKIP_BUILD:-0}" != "1" ]]; then
    if [[ ! -f "$build_dir/CMakeCache.txt" ]]; then
        cmake -S "$root/native" -B "$build_dir" -DCMAKE_BUILD_TYPE=Release
    fi
    cmake --build "$build_dir" --target time_warp_ide -j "$(jobs)"
fi

if [[ ! -x "$binary" ]]; then
    printf 'run.sh: %s was not built\n' "$binary" >&2
    exit 1
fi
exec "$binary" "$@"
