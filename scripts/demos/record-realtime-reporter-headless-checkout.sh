#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
cd "$ROOT_DIR"

OUT_DIR="${OUT_DIR:-target/realtime-reporter-demo}"
if [[ "$OUT_DIR" != /* ]]; then
  OUT_DIR="$ROOT_DIR/$OUT_DIR"
fi
TOOLS_DIR="$OUT_DIR/playwright-tools"
RAW_VIDEO_DIR="$OUT_DIR/raw-video"
LOG_FILE="$OUT_DIR/maven-realtime-demo.log"
OUTPUT_VIDEO="$OUT_DIR/realtime-dashboard-headless-checkout.webm"
PLAYWRIGHT_VERSION="${PLAYWRIGHT_VERSION:-1.57.0}"
DASHBOARD_URL=""

rm -rf "$OUT_DIR"
mkdir -p "$TOOLS_DIR" "$RAW_VIDEO_DIR"

cat > "$TOOLS_DIR/package.json" <<JSON
{"private":true,"type":"commonjs","dependencies":{"playwright":"$PLAYWRIGHT_VERSION"}}
JSON
npm --prefix "$TOOLS_DIR" install --silent
PLAYWRIGHT_BROWSERS_PATH="$OUT_DIR/ms-playwright" npx --prefix "$TOOLS_DIR" playwright install chromium >/dev/null

mvn -q \
  -Dtest=RealtimeReporterHeadlessCheckoutDemoTest \
  -Dgroups=realtime-reporter-demo \
  -Dsurefire.excludedGroups= \
  -Dreporting.realtimeReport.enabled=true \
  -DheadlessExecution=true \
  -DtargetBrowserName=chrome \
  -DautomaticallyAddRecommendedChromeOptions=true \
  test >"$LOG_FILE" 2>&1 &
MVN_PID=$!

cleanup() {
  if kill -0 "$MVN_PID" >/dev/null 2>&1; then
    kill "$MVN_PID" >/dev/null 2>&1 || true
  fi
}
trap cleanup EXIT

for _ in $(seq 1 120); do
  if grep -q "Dashboard started at" "$LOG_FILE"; then
    DASHBOARD_URL="$(grep -oE 'http://localhost:[0-9]+' "$LOG_FILE" | tail -1)"
    break
  fi
  if ! kill -0 "$MVN_PID" >/dev/null 2>&1; then
    wait "$MVN_PID" || true
    echo "Maven finished before the realtime dashboard started. See $LOG_FILE" >&2
    exit 1
  fi
  sleep 1
done

if [[ -z "$DASHBOARD_URL" ]]; then
  echo "Timed out waiting for realtime dashboard. See $LOG_FILE" >&2
  exit 1
fi

TOOLS_DIR="$TOOLS_DIR" \
DASHBOARD_URL="$DASHBOARD_URL" \
RAW_VIDEO_DIR="$RAW_VIDEO_DIR" \
OUTPUT_VIDEO="$OUTPUT_VIDEO" \
PLAYWRIGHT_BROWSERS_PATH="$OUT_DIR/ms-playwright" \
node <<'NODE'
const fs = require('fs');
const path = require('path');
const { chromium } = require(path.join(process.env.TOOLS_DIR, 'node_modules', 'playwright'));

const terminalStatuses = new Set(['PASSED', 'FAILED', 'SKIPPED']);

async function getState(page, dashboardUrl) {
  return await page.evaluate(async (url) => {
    const response = await fetch(`${url}/api/state`, { cache: 'no-store' });
    return response.json();
  }, dashboardUrl);
}

(async () => {
  const browser = await chromium.launch({ headless: true });
  const context = await browser.newContext({
    viewport: { width: 1440, height: 1000 },
    recordVideo: { dir: process.env.RAW_VIDEO_DIR, size: { width: 1440, height: 1000 } },
  });
  const page = await context.newPage();
  await page.goto(process.env.DASHBOARD_URL, { waitUntil: 'domcontentloaded' });
  await page.waitForSelector('body');

  const deadline = Date.now() + 180_000;
  while (Date.now() < deadline) {
    await page.waitForTimeout(1000);
    const state = await getState(page, process.env.DASHBOARD_URL);
    const tests = Array.isArray(state.tests) ? state.tests : [];
    if (tests.length > 0 && tests.every(test => terminalStatuses.has(test.status))) {
      await page.waitForTimeout(5000);
      break;
    }
  }

  const video = page.video();
  if (!video) {
    throw new Error('Playwright did not create a video for the dashboard page.');
  }
  await page.close();
  const videoPath = await video.path();
  await context.close();
  await browser.close();
  fs.mkdirSync(path.dirname(process.env.OUTPUT_VIDEO), { recursive: true });
  fs.copyFileSync(videoPath, process.env.OUTPUT_VIDEO);
  console.log(`Recorded realtime dashboard video: ${process.env.OUTPUT_VIDEO}`);
})().catch(error => {
  console.error(error);
  process.exit(1);
});
NODE

wait "$MVN_PID"
trap - EXIT

if rg -q "failures=\"[1-9]|errors=\"[1-9]" target/surefire-reports/TEST-testPackage.realtime.RealtimeReporterHeadlessCheckoutDemoTest.xml; then
  echo "The realtime checkout demo test reported failures. See $LOG_FILE" >&2
  exit 1
fi

printf 'Realtime dashboard video: %s\nMaven log: %s\n' "$OUTPUT_VIDEO" "$LOG_FILE"
