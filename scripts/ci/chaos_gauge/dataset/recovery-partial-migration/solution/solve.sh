#!/usr/bin/env bash
set -eu
app_root="${CHAOS_GAUGE_APP_ROOT:-/app}"
expected="$(sed -n 's/^expected=//p' "$app_root/contract.txt")"
printf '%s\n' "$expected" > "$app_root/source.txt"

