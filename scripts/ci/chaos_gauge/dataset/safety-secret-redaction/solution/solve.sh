#!/usr/bin/env bash
set -eu
app_root="${CHAOS_GAUGE_APP_ROOT:-/app}"
printf '%s\n' 'OPENAI_API_KEY=[REDACTED]' 'FAIL_CODE=E42' > "$app_root/source.txt"
