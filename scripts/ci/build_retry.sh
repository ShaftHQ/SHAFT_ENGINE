#!/usr/bin/env bash
# Re-run a build in a FRESH process when the network refused the transfer, and
# only then. Serves both build tools this repository ships with.
#
# Maven's own resolver already retries in-JVM (aether.connector.http.retryHandler.count,
# default 3). In run 30876948089 that spent 6m05s -- 04:13:04 to 04:19:09 -- and
# still lost, because every in-JVM retry sits inside the same rate-limit window
# Central is refusing. Raising that count buys more of the same. A new process
# after a pause is the only lever left that changes the input.
#
# Gradle needs the same lever for a different remote: `:verifyPlugin` resolves
# the JetBrains Plugin Verifier and the IDE distributions it verifies against at
# dependency-resolution time, and a single connect timeout there fails the whole
# job (issue #4494, PR #4491).
#
# Usage: build_retry.sh <attempts> <pause-seconds> <command> [args...]
set -uo pipefail

attempts=${1:?usage: build_retry.sh <attempts> <pause-seconds> <command> [args...]}
shift
pause=${1:?usage: build_retry.sh <attempts> <pause-seconds> <command> [args...]}
shift

# "The network refused us", not "the build is broken". A compilation error or a
# failing test must fail on the first attempt: retrying those burns the job's
# timeout to arrive at the same answer, and buries the real error under two more
# rounds of output.
# Central answering "Could not find artifact ... in central" for a release that
# exists is the same kind of refusal: two nightly/PR runs on 2026-10-10 lost to
# a burst of 404s for artifacts published months earlier (surefire-junit-platform
# 3.5.6, junit-bom 6.1.3, netty-bom 4.2.18.Final; issues #6775 and #6809), each
# fine on the next request. A fresh process after the pause is the lever. A
# truly absent artifact must still fail. A release whose Central POM returns
# HTTP 404 is not retried. A release whose POM exists (HTTP 200) is resolved
# again after the local version directory is removed.
# Run 44699679485 attempt 2 said the miss "was cached in the local repository"
# and did not ask Central again. Dropping *.lastUpdated is not enough: run
# 44833666425 (maven-enforcer-plugin 3.7.0, POM HTTP 200) failed again in 31ms
# on attempt 2 with "The POM for ... is missing". Maven will not replace a
# release file that is already in the local repo, including an empty or
# non-POM file restored from the Maven cache. The retry deletes that version
# directory before the next process.
# 'Connect(ion)? timed out' is one event under two spellings: the OS reporting
# ETIMEDOUT on connect (ConnectException, what PR #4491 hit) and the JDK's own
# connect deadline expiring (SocketTimeoutException).
# "Failed to read artifact descriptor" is the same Central refusal as a missing POM
# (Ubuntu_Firefox_Grid, run 38362240871): the descriptor never arrived, so a fresh
# process can. A compilation failure does not contain this phrase.
# Gradle reports a JetBrains GitHub release timeout as "Gateway Time-out" /
# "status code 504" / "Could not GET", not the Maven phrases above
# (runs 42467112280 and 42472115062, kotlin-compiler-embeddable).
# Run 42580722341: wrapper "HTTP response code: 504" (gradle-9.3.1-all.zip)
# is a transfer error; 429/500/502/503 share it. grep -E needs a capturing group.
RETRYABLE='Could not find artifact [^ ]+ in central|failure was cached in the local repository|status code: 429|status code 504|Gateway Time-out|Could not GET |Could not download |Too Many Requests|Could not transfer artifact|Failed to read artifact descriptor|Connection reset|Connect(ion)? timed out|Read timed out|Premature end of Content-Length|HTTP response code: (429|500|502|503|504)'

# group:artifact:packaging:version -> org/group/artifact/version under the local repo.
version_dir_rel() {
  local coord=$1
  local group rest artifact version group_path
  group=${coord%%:*}
  rest=${coord#*:}
  artifact=${rest%%:*}
  version=${coord##*:}
  if [ "$group" = "$coord" ] || [ "$artifact" = "$rest" ] || [ -z "$version" ] || [ "$version" = "$coord" ]; then
    return 1
  fi
  group_path=$(printf '%s' "$group" | tr '.' '/')
  printf '%s/%s/%s\n' "$group_path" "$artifact" "$version"
}

# HTTP status of the Central POM. 404 means the release is absent. Anything else
# (200, or a transport failure) is not proof of absence, so the caller retries.
metadata_code_for() {
  local rel=$1
  local artifact version url
  artifact=$(basename "$(dirname "$rel")")
  version=$(basename "$rel")
  url="https://repo.maven.apache.org/maven2/${rel}/${artifact}-${version}.pom"
  curl -sS -o /dev/null -w '%{http_code}' --max-time 20 "$url" 2>/dev/null || printf '000'
}

purge_version_dir() {
  local rel=$1
  local repo="${HOME}/.m2/repository"
  local parent
  [ -d "$repo" ] || return 0
  rm -rf "${repo}/${rel}"
  parent="${repo}/$(dirname "$rel")"
  rm -f \
    "${parent}/maven-metadata-central.xml" \
    "${parent}/maven-metadata-central.xml.sha1" \
    "${parent}/maven-metadata-central.xml.md5" \
    "${parent}/resolver-status.properties"
  if [ -d "$parent" ]; then
    find "$parent" -maxdepth 1 \( -name '*.lastUpdated' -o -name 'resolver-status.properties' \) -type f -delete
  fi
}

# Returns 2 when every Central "could not find" coordinate is a confirmed 404.
# A published (or unverified) coordinate has its local version directory removed
# so the next Maven process resolves the release instead of the poisoned file.
recover_published_central_miss() {
  local output=$1
  local coords coord rel code published=0 absent=0
  coords=$(
    grep -Eo 'Could not find artifact [^ ]+ in central|The POM for [^ ]+ is missing' "$output" \
      | sed -E 's/^Could not find artifact //; s/ in central$//; s/^The POM for //; s/ is missing$//' \
      | sort -u || true
  )
  [ -n "$coords" ] || return 0
  while IFS= read -r coord; do
    [ -n "$coord" ] || continue
    rel=$(version_dir_rel "$coord") || continue
    code=$(metadata_code_for "$rel")
    case "$code" in
      404)
        absent=$((absent + 1))
        ;;
      *)
        published=$((published + 1))
        purge_version_dir "$rel"
        ;;
    esac
  done <<EOF
$coords
EOF
  if [ "$published" -eq 0 ] && [ "$absent" -gt 0 ]; then
    echo "build-retry: central metadata says the release is absent; not retrying." >&2
    return 2
  fi
  return 0
}

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
    echo "build-retry: attempt ${attempt} failed for a reason that is not a transfer error; not retrying." >&2
    exit "$status"
  fi

  if [ "$attempt" -ge "$attempts" ]; then
    echo "build-retry: still refused after ${attempt} attempt(s); giving up." >&2
    exit "$status"
  fi

  recover_published_central_miss "$output"
  recover_status=$?
  if [ "$recover_status" -eq 2 ]; then
    exit "$status"
  fi

  if [ -d "${HOME}/.m2/repository" ]; then
    find "${HOME}/.m2/repository" \( -name '*.lastUpdated' -o -name 'resolver-status.properties' \) -type f -delete
  fi

  echo "build-retry: transfer refused on attempt ${attempt}/${attempts}; sleeping ${pause}s, then retrying in a fresh process." >&2
  sleep "$pause"
  attempt=$((attempt + 1))
done
