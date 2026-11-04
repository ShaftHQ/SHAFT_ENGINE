#!/usr/bin/env bash
# Thin wrapper so docs can keep calling bash …/check_abstraction.sh
set -euo pipefail
ROOT="$(cd "$(dirname "$0")" && pwd)"
exec python3 "$ROOT/check_abstraction.py" "$@"
