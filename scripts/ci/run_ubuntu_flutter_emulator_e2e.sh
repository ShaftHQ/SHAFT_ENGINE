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
appium --address 127.0.0.1 --port 4723 --log-level info --log "$appiumLog" &
APPIUM_PID=$!
trap 'kill "$APPIUM_PID" 2>/dev/null || true' EXIT

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
adb uninstall com.example.appium_testing_app || true

TEST_SELECTOR="${FLUTTER_TEST_SELECTOR:-testPackage.appium.FlutterTest}"
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
  "-DgenerateAllureReportArchive=true"
if [ ! -d "$GITHUB_WORKSPACE/shaft-engine/allure-results" ]; then
  for d in "$PWD/shaft-engine/allure-results" "$PWD/allure-results" "$GITHUB_WORKSPACE/allure-results"; do
    if [ -n "$(find "$d" -name '*-result.json' -type f -print -quit 2>/dev/null)" ]; then
      mkdir -p "$GITHUB_WORKSPACE/shaft-engine/allure-results"
      cp -a "$d"/. "$GITHUB_WORKSPACE/shaft-engine/allure-results/"
      break
    fi
  done
fi
