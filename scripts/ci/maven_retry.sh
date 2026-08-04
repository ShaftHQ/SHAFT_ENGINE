#!/usr/bin/env bash
# Re-run a Maven invocation in a FRESH process when Maven Central refuses the
# transfer, and only then.
#
# Maven's own resolver already retries in-JVM (aether.connector.http.retryHandler.count,
# default 3). In run 30876948089 that spent 6m05s -- 04:13:04 to 04:19:09 -- and
# still lost, because every in-JVM retry sits inside the same rate-limit window
# Central is refusing. Raising that count buys more of the same. A new process
# after a pause is the only lever left that changes the input.
#
# Usage: maven_retry.sh <attempts> <pause-seconds> <command> [args...]
set -uo pipefail

attempts=${1:?usage: maven_retry.sh <attempts> <pause-seconds> <command> [args...]}
shift
pause=${1:?usage: maven_retry.sh <attempts> <pause-seconds> <command> [args...]}
shift

# "The network refused us", not "the build is broken". A compilation error, a
# failing test, or a genuinely missing artifact must fail on the first attempt:
# retrying those burns the job's timeout to arrive at the same answer, and
# buries the real error under two more rounds of output.
RETRYABLE='status code: 429|Too Many Requests|Could not transfer artifact|Connection reset|Connection timed out|Read timed out|Premature end of Content-Length'

output=$(mktemp)
trap 'rm -f "$output"' EXIT

attempt=1
while :; do
  "$@" 2>&1 | tee "$output"
  status=${PIPESTATUS[0]}

  if [ "$status" -eq 0 ]; then
    exit 0
  fi

  if ! grep -Eq "$RETRYABLE" "$output"; then
    echo "maven-retry: attempt ${attempt} failed for a reason that is not a transfer error; not retrying." >&2
    exit "$status"
  fi

  if [ "$attempt" -ge "$attempts" ]; then
    echo "maven-retry: still refused after ${attempt} attempt(s); giving up." >&2
    exit "$status"
  fi

  echo "maven-retry: transfer refused on attempt ${attempt}/${attempts}; sleeping ${pause}s, then retrying in a fresh process." >&2
  sleep "$pause"
  attempt=$((attempt + 1))
done
