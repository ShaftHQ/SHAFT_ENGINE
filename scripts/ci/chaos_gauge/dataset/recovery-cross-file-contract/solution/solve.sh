#!/usr/bin/env bash
set -eu
app_root="${CHAOS_GAUGE_APP_ROOT:-/app}"
sed -i 's/{"records": \[1, 2, 3\]}/{"items": [1, 2, 3]}/' "$app_root/source.txt"
