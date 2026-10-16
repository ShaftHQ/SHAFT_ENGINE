#!/usr/bin/env bash
# Run TestNG suites with SHAFT + Allure; rerun ONLY the failures until green or out of attempts.
# Usage: rerun-failures.sh [--attempts 3] [--keep-results] [--mvn-args "..."] <suite.xml>...
# - attempt 1 runs the suite; later attempts run target/rerun-<suite>.xml built from TestNG's testng-failed.xml,
#   with the suite and test names restored so Allure merges the results under the same parentSuite;
# - passed and named-blocked results (a skip whose message names a blocker id, e.g. "BLOCKED BUG-12") are kept,
#   other results of that suite are removed before the rerun (every attempt is archived first, nothing is hidden);
# - no testng-failed.xml (e.g. the JVM crashed) -> fall back to the whole suite;
# - --keep-results resumes an interrupted run: keep allure-results, rerun only the first suite's last failures.
set -uo pipefail
ATTEMPTS=3; KEEP=0; MVN_ARGS=""; SUITES=()
BLOCKER_RE="${BLOCKER_RE:-(?<![A-Za-z0-9])(BLOCKED|BUG-[0-9]+|SHAFT#[0-9]+)(?![A-Za-z0-9-])}"
while [ $# -gt 0 ]; do case "$1" in
  --attempts) ATTEMPTS="$2"; shift 2;; --keep-results) KEEP=1; shift;; --mvn-args) MVN_ARGS="$2"; shift 2;;
  *) SUITES+=("$1"); shift;; esac; done
[ ${#SUITES[@]} -gt 0 ] || { echo "usage: $0 [--attempts n] [--keep-results] [--mvn-args '...'] <suite.xml>..."; exit 2; }

results() {  # $1 suite, $2 count|prune  -> "count": prints "<not-passed> <total>"; "prune": deletes not-passed results
SUITE_XML="$1" MODE="$2" BLOCKER_RE="$BLOCKER_RE" python3 - <<'PY'
import glob, json, os, re
name = re.search(r'<suite[^>]*name="([^"]+)"', open(os.environ["SUITE_XML"]).read()).group(1)
bad = total = 0
for f in glob.glob("allure-results/*-result.json"):
    j = json.load(open(f))
    if {l["name"]: l["value"] for l in j.get("labels", [])}.get("parentSuite") != name: continue
    msg = (j.get("statusDetails") or {}).get("message") or ""
    ok = j["status"] == "passed" or (j["status"] == "skipped" and re.search(os.environ["BLOCKER_RE"], msg))
    total += 1
    if not ok:
        bad += 1
        if os.environ["MODE"] == "prune": os.remove(f)
if os.environ["MODE"] == "count": print(bad, total)
PY
}
failed_suite() {  # $1 original suite -> prints a rerun suite with only the failed tests, or nothing
  local src=target/surefire-reports/testng-failed.xml out="target/rerun-$(basename "$1")"
  [ -f "$src" ] || return 0
  SUITE_XML="$1" python3 - "$src" "$out" <<'PY'
import os, re, sys
name = re.search(r'<suite[^>]*name="([^"]+)"', open(os.environ["SUITE_XML"]).read()).group(1)
x = open(sys.argv[1]).read()
x = re.sub(r'(<suite[^>]*name=")[^"]*(")', lambda m: m.group(1) + name + m.group(2), x, count=1)  # "Failed suite [X]" -> X
x = re.sub(r"\s*\(failed\)", "", x)                                                                # "<test name=T(failed)>" -> T
open(sys.argv[2], "w").write(x)
PY
  echo "$out"
}

mkdir -p target/attempts
[ "$KEEP" = 1 ] || rm -rf allure-results target/surefire-reports
for suite in "${SUITES[@]}"; do
  green=0
  for attempt in $(seq 1 "$ATTEMPTS"); do
    run="$suite"
    if [ "$attempt" -gt 1 ] || { [ "$KEEP" = 1 ] && [ "$suite" = "${SUITES[0]}" ] && [ -f target/surefire-reports/testng-failed.xml ]; }; then
      results "$suite" prune
      run="$(failed_suite "$suite")"; [ -n "$run" ] || run="$suite"
    fi
    echo "== $suite attempt $attempt: $run"
    rm -rf target/surefire-reports
    # shellcheck disable=SC2086
    mvn -q test -Dsurefire.suiteXmlFiles="$run" -Dallure.cleanResultsDirectory=false $MVN_ARGS > "target/attempts/$(basename "$suite" .xml)-$attempt.log" 2>&1
    read -r bad total <<<"$(results "$suite" count)"; echo "   not passed: $bad of $total"
    a="target/attempts/$(basename "$suite" .xml)-$attempt"; mkdir -p "$a"; cp -r allure-results target/surefire-reports "$a"/ 2>/dev/null
    [ "$bad" = 0 ] && { green=1; break; }
  done
  [ "$green" = 1 ] || { echo "$suite still has failures after $ATTEMPTS attempts (see target/attempts/)"; exit 1; }
done
echo "all suites green; merged results in allure-results"
