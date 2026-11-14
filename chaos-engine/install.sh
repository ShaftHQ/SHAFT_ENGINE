#!/usr/bin/env sh
# ChaosEngine moved to https://github.com/ShaftHQ/chaosengine-harness.
# This shim keeps the old SHAFT_ENGINE one-liner working: it forwards every
# argument to the installer on chaosengine-harness main.
set -eu
url="https://raw.githubusercontent.com/ShaftHQ/chaosengine-harness/main/install.sh"
case "${1:-}" in
  https://raw.githubusercontent.com/ShaftHQ/SHAFT_ENGINE/*) shift ;;
esac
echo "ChaosEngine moved to ShaftHQ/chaosengine-harness; forwarding to $url" >&2
script=$(mktemp "${TMPDIR:-/tmp}/chaos-engine-install.XXXXXX")
trap 'rm -f "$script"' EXIT
curl -fsSL "$url" -o "$script"
bash "$script" "$url" "$@"
