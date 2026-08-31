#!/usr/bin/env bash
set -eu
app_root="${CHAOS_GAUGE_APP_ROOT:-/app}"
sed -i 's/settings = load("current.conf")/settings = {**load("legacy.conf"), **load("current.conf")}/' "$app_root/source.txt"
