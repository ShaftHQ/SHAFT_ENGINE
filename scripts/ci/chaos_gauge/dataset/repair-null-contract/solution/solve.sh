#!/usr/bin/env bash
set -eu
app_root="${CHAOS_GAUGE_APP_ROOT:-/app}"
sed -i 's/record\["label"\]\.strip()/(record["label"] or "unknown").strip()/' "$app_root/source.txt"
