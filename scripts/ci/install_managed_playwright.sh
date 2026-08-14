#!/usr/bin/env bash
set -euo pipefail

: "${RUNNER_TEMP:?RUNNER_TEMP must identify the isolated CI temporary directory}"

cli_jar="$(find shaft-cli/target -maxdepth 1 -name 'shaft-cli-*[0-9].jar' -print -quit)"
test -n "$cli_jar"

cache_root="$RUNNER_TEMP/shaft-playwright-cache"
data_root="$cache_root/data"
plan="$RUNNER_TEMP/shaft-playwright-plan.json"

java -jar "$cli_jar" setup plan --profile PLAYWRIGHT --mode MANAGED \
  --output "$plan" --cache-root "$cache_root" --data-root "$data_root"
digest="$(python3 -c "import json,sys; print(json.load(open(sys.argv[1], encoding='utf-8'))['digest'])" "$plan")"
java -jar "$cli_jar" setup install --plan "$plan" --approve "$digest" \
  --cache-root "$cache_root" --data-root "$data_root"
java -jar "$cli_jar" setup verify --profile PLAYWRIGHT --mode MANAGED --json \
  --cache-root "$cache_root" --data-root "$data_root"
test -s "$data_root/receipts/playwright.json"
