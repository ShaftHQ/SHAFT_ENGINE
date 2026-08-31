#!/usr/bin/env bash
set -eu
app_root="${CHAOS_GAUGE_APP_ROOT:-/app}"
printf '%s\n' '{"affected": true, "broadCampaign": false, "focused": true}' > "$app_root/source.txt"
