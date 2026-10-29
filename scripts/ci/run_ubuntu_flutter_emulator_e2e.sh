#!/usr/bin/env bash
# Single invocation for android-emulator-runner: that action runs each YAML
# script line as a separate `sh -c`, so APK_PATH=... then ls "$APK_PATH" cannot
# live on adjacent YAML lines.
set -eu
cd "$GITHUB_WORKSPACE"
APK_PATH="$GITHUB_WORKSPACE/shaft-engine/src/test/resources/testDataFiles/apps/flutter-demo.apk"
ls -l "$APK_PATH"
adb devices

# Defense in depth: preflight should have written pin bin to $GITHUB_PATH, but
# re-resolve the pin if this fresh emulator-runner shell still lacks appium.
if ! command -v appium >/dev/null 2>&1; then
  pin="$(cd "$(dirname "$0")/npm-pins/appium-3.5.2" && pwd)"
  export PATH="$pin/node_modules/.bin:$PATH"
fi
if ! command -v appium >/dev/null 2>&1; then
  echo "::error::appium not on PATH; run scripts/ci/flutter_emulator_preflight.sh first (and ensure pin bin is on \$GITHUB_PATH)."
  exit 1
fi

appiumLog="$RUNNER_TEMP/appium-server.log"
mavenLog="$RUNNER_TEMP/flutter-maven.log"
APPIUM_PID=""
trap 'kill "$APPIUM_PID" 2>/dev/null || true' EXIT

start_appium() {
  if [ -n "$APPIUM_PID" ]; then
    kill "$APPIUM_PID" 2>/dev/null || true
    wait "$APPIUM_PID" 2>/dev/null || true
  fi
  appium --address 127.0.0.1 --port 4723 --log-level info --log "$appiumLog" &
  APPIUM_PID=$!
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

# #7496: device primary under time-cap; authorized fallback = one Appium restart path
DEVICE_CAP_S="${CHAOS_ENGINE_DEVICE_CAP_S:-900}"
export CHAOS_ENGINE_DEVICE_CAP_S="$DEVICE_CAP_S"
if attempt_flutter_test; then
  :
elif grep -q "socket hang up" "$mavenLog"; then
  echo "::warning::Flutter Appium proxy dropped the session. Restarting Appium once (device fallback)."
  # Record fallback via mobile_retro_runtime (picklable spawn-safe worker)
  python3 scripts/ci/mobile_retro_runtime.py device-fallback --mode error --error-msg "socket hang up" --fallback restarted --cap-s 5 >/dev/null || true
  attempt_flutter_test
elif grep -qiE "ClassNotFound|server-side gate|versionCode ignored" "$mavenLog"; then
  sig=$(tr '\n' ' ' <"$mavenLog" | head -c 4000)
  python3 scripts/ci/mobile_retro_runtime.py classify-before-rebuild --signature "$sig" || {
    echo "::error::Flutter run Blocked by backend gate; not rebuilding device path."
    exit 2
  }
  exit 1
else
  exit 1
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
