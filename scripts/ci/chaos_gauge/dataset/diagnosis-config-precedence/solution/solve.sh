#!/usr/bin/env bash
set -eu
app_root="${CHAOS_GAUGE_APP_ROOT:-/app}"
sed -i 's/\["override.ini", "default.ini"\]/["default.ini", "override.ini"]/' "$app_root/source.txt"
