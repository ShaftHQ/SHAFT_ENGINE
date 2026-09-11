#!/usr/bin/env bash
# Wave F (#5732) proof runner: flake-prone Wave B/C unit subset N times + microbench.
# Usage:
#   scripts/ci/run_click_type_wave_f_proof.sh [N]
# Default N=3. Headless Chrome required for ClickTypeMicrobenchTest browser cases.
set -euo pipefail

ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
cd "$ROOT"

N="${1:-3}"
if ! [[ "$N" =~ ^[0-9]+$ ]] || [[ "$N" -lt 1 ]]; then
  echo "N must be a positive integer (got: $N)" >&2
  exit 2
fi

MVN_BIN="${MVN_BIN:-mvn}"
# Fail closed: shaft-engine Surefire hardcodes testFailureIgnore=true so JaCoCo can
# still report; Maven exit 0 alone is not proof. We also parse Surefire/TestNG XML.
COMMON_ARGS=(
  -pl shaft-engine
  -Dallure.automaticallyOpen=false
  -DheadlessExecution=true
  -DexecutionAddress=local
  -DtargetBrowserName=chrome
  -DfailIfNoTests=false
  -Dmaven.test.failure.ignore=false
)

UNIT_TESTS="ElementClassifierTest,InteractionStrategiesActionsTest,PlaywrightInteractionStrategiesUnitTest"
MICRO_TESTS="ClickTypeMicrobenchTest"
REPORT_ROOT="${ROOT}/shaft-engine/target/surefire-reports"

assert_surefire_green() {
  local label="$1"
  python3 - "$REPORT_ROOT" "$label" <<'PY'
import pathlib
import re
import sys

report_root = pathlib.Path(sys.argv[1])
label = sys.argv[2]
failures = 0
errors = 0

surefire = list(report_root.glob("TEST-*.xml"))
if surefire:
    for path in surefire:
        text = path.read_text(encoding="utf-8", errors="replace")
        fm = re.search(r'\bfailures="(\d+)"', text)
        em = re.search(r'\berrors="(\d+)"', text)
        failures += int(fm.group(1)) if fm else 0
        errors += int(em.group(1)) if em else 0
else:
    testng = report_root / "testng-results.xml"
    if not testng.is_file():
        print(f"FAIL: {label} produced no Surefire/TestNG report under {report_root}", file=sys.stderr)
        sys.exit(1)
    text = testng.read_text(encoding="utf-8", errors="replace")
    fm = re.search(r'<testng-results[^>]*\bfailed="(\d+)"', text)
    failures = int(fm.group(1)) if fm else 0

if failures or errors:
    print(f"FAIL: {label} reported failures={failures} errors={errors}", file=sys.stderr)
    sys.exit(1)
print(f"OK: {label} green (failures=0 errors=0)")
PY
}

echo "=== Wave F flake proof: unit subset x${N} (${UNIT_TESTS}) ==="
for ((i = 1; i <= N; i++)); do
  echo "--- unit run ${i}/${N} ---"
  "${MVN_BIN}" "${COMMON_ARGS[@]}" -Dtest="${UNIT_TESTS}" test
  assert_surefire_green "unit run ${i}/${N}"
done

echo "=== Wave F microbench (strategy path + classifier overhead) ==="
"${MVN_BIN}" "${COMMON_ARGS[@]}" -Dtest="${MICRO_TESTS}" test
assert_surefire_green "ClickTypeMicrobenchTest"

echo "=== Wave F proof complete (N=${N}) ==="
echo "Waves D (mobile) / E (desktop UIA matrix): N/A for this PR — web proof only; see epic #5732."
