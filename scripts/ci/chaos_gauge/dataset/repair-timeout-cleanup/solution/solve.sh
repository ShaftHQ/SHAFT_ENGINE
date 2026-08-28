#!/usr/bin/env bash
set -eu
app_root="${CHAOS_GAUGE_APP_ROOT:-/app}"
printf '\nlock.unlink()\n' >> "$app_root/source.txt"
