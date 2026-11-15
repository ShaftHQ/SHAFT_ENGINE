#!/usr/bin/env bash
# #8057 e2c: give CI jobs the ChaosEngine harness source as .chaos-engine/ so
# protected suites (delete guard, secret guard, client-name scrub, hooks) run
# against real code instead of being quarantined. Fetches one pinned commit of
# ShaftHQ/chaosengine-harness; never runs the installer or provisions tools.
set -euo pipefail
root="$(git rev-parse --show-toplevel)"
pin_file="$root/scripts/ci/harness_pin.txt"
pin="$(grep -v '^#' "$pin_file" | tr -d '[:space:]')"
if [[ ! "$pin" =~ ^[0-9a-f]{40}$ ]]; then
  echo "::error::$pin_file must hold one 40-hex commit, got '$pin'" >&2
  exit 1
fi
dest="$root/.chaos-engine"
# -e misses a dangling symlink, which would still make git init write through it.
if [[ -e "$dest" || -L "$dest" ]]; then
  echo "::error::$dest already exists; refusing to overwrite an installed overlay" >&2
  exit 1
fi
git init -q "$dest"
# Reject a corrupt fetch, ignore the fetched repo's hooks, and never prompt.
GIT_TERMINAL_PROMPT=0 git -C "$dest" \
  -c fetch.fsckObjects=true \
  -c core.hooksPath=/dev/null \
  fetch -q --depth 1 https://github.com/ShaftHQ/chaosengine-harness.git "$pin"
git -C "$dest" -c advice.detachedHead=false -c core.hooksPath=/dev/null checkout -q FETCH_HEAD
actual="$(git -C "$dest" rev-parse HEAD)"
if [[ "$actual" != "$pin" ]]; then
  echo "::error::fetched $actual, expected pinned $pin" >&2
  rm -rf "$dest"
  exit 1
fi
echo "ChaosEngine harness overlay at $pin -> .chaos-engine/"
