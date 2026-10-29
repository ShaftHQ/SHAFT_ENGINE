#!/usr/bin/env bash
# Single invocation for android-emulator-runner: that action runs each YAML
# script line as a separate `sh -c`, so APK_PATH=... then ls "$APK_PATH" cannot
# live on adjacent YAML lines.
set -eu
cd "$GITHUB_WORKSPACE"
APK_PATH="$GITHUB_WORKSPACE/shaft-engine/src/test/resources/testDataFiles/apps/flutter-demo.apk"
ls -l "$APK_PATH"
adb devices

appiumLog="$RUNNER_TEMP/appium-server.log"
mavenLog="$RUNNER_TEMP/flutter-maven.log"
appiumPidFile="$RUNNER_TEMP/flutter-appium.pid"
APPIUM_PID=""
# Pidfile lets a `timeout` child update Appium's pid for the parent EXIT trap (#7496).
trap 'kill "$(cat "$appiumPidFile" 2>/dev/null || echo "$APPIUM_PID")" 2>/dev/null || true' EXIT
export appiumLog mavenLog appiumPidFile APK_PATH GITHUB_WORKSPACE

start_appium() {
  if [ -f "$appiumPidFile" ]; then
    old_pid="$(cat "$appiumPidFile" 2>/dev/null || true)"
    if [ -n "$old_pid" ]; then
      kill "$old_pid" 2>/dev/null || true
      wait "$old_pid" 2>/dev/null || true
    fi
  elif [ -n "$APPIUM_PID" ]; then
    kill "$APPIUM_PID" 2>/dev/null || true
    wait "$APPIUM_PID" 2>/dev/null || true
  fi
  appium --address 127.0.0.1 --port 4723 --log-level info --log "$appiumLog" &
  APPIUM_PID=$!
  echo "$APPIUM_PID" >"$appiumPidFile"
  ready=false
  for i in $(seq 1 60); do
    if curl --max-time 2 -fsS http://127.0.0.1:4723/status >/dev/null 2>&1; then
      ready=true
      break
    fi
    sleep 1
  done
  if [ "$ready" != true ]; then
    echo "::error::Appium server did not become ready."
    cat "$appiumLog" || true
    exit 1
  fi
}

open_flutter_session() {
  SESSION_PAYLOAD=$(jq -n --arg app "$APK_PATH" '{capabilities: {alwaysMatch: {platformName: "Android", "appium:automationName": "FlutterIntegration", "appium:app": $app, "appium:noReset": true}}}')
  RESPONSE=$(curl --max-time 120 -fsS -X POST -H "Content-Type: application/json" -d "$SESSION_PAYLOAD" http://127.0.0.1:4723/session)
  echo "$RESPONSE"
  SESSION_ID=$(echo "$RESPONSE" | jq -r '.value.sessionId // empty')
  if [ -z "$SESSION_ID" ]; then
    echo "::error::Appium session did not open -- no sessionId in response."
    cat "$appiumLog" || true
    exit 1
  fi
  echo "Appium Flutter session opened: $SESSION_ID"
  curl --max-time 30 -fsS -X DELETE "http://127.0.0.1:4723/session/$SESSION_ID" || true
  # Preflight uses noReset, so the demo app can stay warm without a Flutter server for Maven.
  # CI-built APK uses appium_testing_app; older checked-in APK used demo_app — uninstall both.
  adb uninstall com.example.appium_testing_app || true
  adb uninstall com.example.demo_app || true
}

run_flutter_maven() {
  TEST_SELECTOR="${FLUTTER_TEST_SELECTOR:-testPackage.appium.FlutterTest}"
  set +e
  mvn -f "$GITHUB_WORKSPACE/pom.xml" -pl shaft-engine -e test \
    "-Dtest=${TEST_SELECTOR}" \
    "-Dshaft.enableFlutterE2E=true" \
    "-DexecutionAddress=127.0.0.1:4723" \
    "-Dmobile_app=src/test/resources/testDataFiles/apps/flutter-demo.apk" \
    "-Dmobile_automationName=FlutterIntegration" \
    "-DtargetOperatingSystem=android" \
    "-DdefaultElementIdentificationTimeout=60" \
    "-Dallure.automaticallyOpen=false" \
    "-DheadlessExecution=true" \
    "-DgenerateAllureReportArchive=true" \
    >"$mavenLog" 2>&1
  mavenStatus=$?
  set -e
  cat "$mavenLog"
  return "$mavenStatus"
}

attempt_flutter_test() {
  start_appium
  open_flutter_session
  run_flutter_maven
}

# #7496: wall-clock time-cap the real Appium+Maven primary; authorized fallback =
# one Appium restart + retry under the same DEVICE_CAP_S (not a post-hoc --mode error no-op).
DEVICE_CAP_S="${CHAOS_ENGINE_DEVICE_CAP_S:-900}"
export CHAOS_ENGINE_DEVICE_CAP_S="$DEVICE_CAP_S"
export -f start_appium open_flutter_session run_flutter_maven attempt_flutter_test

set +e
# GNU timeout: exit 124 when the primary exceeds DEVICE_CAP_S.
timeout --signal=TERM --kill-after=60s "$DEVICE_CAP_S" bash -c 'attempt_flutter_test'
primary_rc=$?
set -e

if [ "$primary_rc" -eq 0 ]; then
  :
elif [ "$primary_rc" -eq 124 ] || { [ -f "$mavenLog" ] && grep -q "socket hang up" "$mavenLog"; }; then
  echo "::warning::Flutter primary hit DEVICE_CAP_S=${DEVICE_CAP_S}s or Appium socket hang up. One authorized device fallback (Appium restart + retry under the same cap)."
  # Authorized fallback = real retry under the same wall-clock DEVICE_CAP_S (library time-cap covered by unit tests).
  set +e
  timeout --signal=TERM --kill-after=60s "$DEVICE_CAP_S" bash -c 'attempt_flutter_test'
  fallback_rc=$?
  set -e
  if [ "$fallback_rc" -ne 0 ]; then
    exit "$fallback_rc"
  fi
elif [ -f "$mavenLog" ] && grep -qiE "ClassNotFound|server-side gate|versionCode ignored" "$mavenLog"; then
  sig=$(tr '\n' ' ' <"$mavenLog" | head -c 4000)
  python3 scripts/ci/mobile_retro_runtime.py classify-before-rebuild --signature "$sig" || {
    echo "::error::Flutter run Blocked by backend gate; not rebuilding device path."
    exit 2
  }
  exit 1
else
  exit "$primary_rc"
fi
# #7494: freeze green allure cohort when present
if [ -d "$GITHUB_WORKSPACE/shaft-engine/allure-results" ] || [ -d allure-results ]; then
  results_dir="$GITHUB_WORKSPACE/shaft-engine/allure-results"
  [ -d "$results_dir" ] || results_dir="allure-results"
  python3 scripts/ci/mobile_retro_runtime.py after-green-allure --root "$GITHUB_WORKSPACE" --results "$results_dir" --green || true
fi
if [ ! -d "$GITHUB_WORKSPACE/shaft-engine/allure-results" ]; then
  for d in "$PWD/shaft-engine/allure-results" "$PWD/allure-results" "$GITHUB_WORKSPACE/allure-results"; do
    if [ -n "$(find "$d" -name '*-result.json' -type f -print -quit 2>/dev/null)" ]; then
      mkdir -p "$GITHUB_WORKSPACE/shaft-engine/allure-results"
      cp -a "$d"/. "$GITHUB_WORKSPACE/shaft-engine/allure-results/"
      break
    fi
  done
fi
