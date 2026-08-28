#!/usr/bin/env bash
set -eu
app_root="${CHAOS_GAUGE_APP_ROOT:-/app}"
sed -i 's/frames\[0\]/frames[-1]/' "$app_root/source.txt"
