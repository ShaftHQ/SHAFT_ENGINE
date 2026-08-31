#!/usr/bin/env bash
set -eu
app_root="${CHAOS_GAUGE_APP_ROOT:-/app}"
sed -i '/foreign-notes.txt/d' "$app_root/source.txt"
