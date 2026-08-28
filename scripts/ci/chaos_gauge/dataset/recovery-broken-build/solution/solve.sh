#!/usr/bin/env bash
set -eu
app_root="${CHAOS_GAUGE_APP_ROOT:-/app}"
sed -i 's/from maths/from helpers/' "$app_root/source.txt"
