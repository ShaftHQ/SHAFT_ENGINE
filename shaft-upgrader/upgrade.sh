#!/usr/bin/env sh
set -eu
SCRIPT_DIR=$(CDPATH= cd -- "$(dirname -- "$0")" && pwd)
fail() { echo "shaft-upgrader: $1" >&2; exit "${2:-1}"; }
find_python() {
  if command -v python3 >/dev/null 2>&1; then
    command -v python3
    return 0
  fi
  if command -v python >/dev/null 2>&1; then
    command -v python
    return 0
  fi
  fail "python3 is required to run the SHAFT Engine project upgrader." 3
}
PYTHON=$(find_python)
exec "$PYTHON" "$SCRIPT_DIR/upgrade_to_modular_shaft.py" "$@"
