#!/usr/bin/env sh
set -eu
SCRIPT_DIR=$(CDPATH= cd -- "$(dirname -- "$0")" && pwd)
fail() { echo "shaft-upgrader: $1" >&2; exit "${2:-1}"; }
python_ok() {
  "$1" -c 'import sys; raise SystemExit(0 if sys.version_info >= (3, 9) else 1)' >/dev/null 2>&1
}
find_python() {
  for name in python3 python; do
    candidate=$(command -v "$name" 2>/dev/null || true)
    if [ -n "$candidate" ] && python_ok "$candidate"; then
      echo "$candidate"
      return 0
    fi
  done
  fail "Python 3.9 or newer is required to run the SHAFT Engine project upgrader." 3
}
PYTHON=$(find_python)
exec "$PYTHON" "$SCRIPT_DIR/upgrade_to_modular_shaft.py" "$@"
