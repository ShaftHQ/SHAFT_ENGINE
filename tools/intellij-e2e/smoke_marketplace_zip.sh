#!/usr/bin/env bash
# Marketplace ZIP → cold IntelliJ IDEA Community smoke (#7597 / epic #7568).
# Downloads IDEA IC when IDEA_HOME is unset, sideloads build/distributions/*.zip,
# launches under xvfb, asserts PluginManager loaded SHAFT, then runs a classpath
# core-smoke against the installed JAR. Does not open Allure or browsers.
set -euo pipefail
ROOT="$(cd "$(dirname "$0")/../.." && pwd)"
PLUGIN_DIR="$ROOT/shaft-intellij"
SANDBOX="${SMOKE_SANDBOX:-/tmp/shaft-intellij-e2e-sandbox}"
EV="$SANDBOX/evidence"
JAVA_HOME="${JAVA_HOME:-}"
if [[ -z "$JAVA_HOME" || ! -x "$JAVA_HOME/bin/java" ]]; then
  for c in /workspace/jdk-21 "$HOME/.sdkman/candidates/java/current"; do
    if [[ -x "$c/bin/java" ]]; then JAVA_HOME=$c; break; fi
  done
fi
export JAVA_HOME PATH="$JAVA_HOME/bin:$PATH"
command -v xvfb-run >/dev/null || { echo "xvfb-run required"; exit 2; }

mkdir -p "$SANDBOX" "$EV"
cd "$PLUGIN_DIR"
./gradlew buildPlugin --no-daemon
ZIP=$(ls -1t build/distributions/shaft-intellij-*.zip | head -1)
cp -f "$ZIP" "$EV/shaft-intellij-marketplace.zip"
sha256sum "$ZIP" | tee "$EV/marketplace-zip.sha256"

if [[ -z "${IDEA_HOME:-}" ]]; then
  URL=$(python3 - <<'PY'
import json,urllib.request
data=json.load(urllib.request.urlopen('https://data.services.jetbrains.com/products?code=IIC&release.type=release', timeout=60))
for rel in data[0]['releases']:
  if rel.get('version','').startswith('2024.3'):
    print(rel['downloads']['linux']['link']); break
else:
  print(data[0]['releases'][0]['downloads']['linux']['link'])
PY
)
  curl -L --fail --retry 3 -o "$SANDBOX/ideaIC.tar.gz" "$URL"
  rm -rf "$SANDBOX/idea-IC" "$SANDBOX/extract"
  mkdir -p "$SANDBOX/extract"
  tar -xzf "$SANDBOX/ideaIC.tar.gz" -C "$SANDBOX/extract"
  mv "$SANDBOX/extract"/idea-IC* "$SANDBOX/idea-IC"
  IDEA_HOME="$SANDBOX/idea-IC"
fi

rm -rf "$SANDBOX/config" "$SANDBOX/system" "$SANDBOX/log" "$SANDBOX/plugins" "$SANDBOX/project"
mkdir -p "$SANDBOX/config" "$SANDBOX/system" "$SANDBOX/log" "$SANDBOX/plugins" "$SANDBOX/project"
unzip -qo "$ZIP" -d "$SANDBOX/plugins"
cat > "$SANDBOX/idea.properties" <<PROP
idea.config.path=$SANDBOX/config
idea.system.path=$SANDBOX/system
idea.log.path=$SANDBOX/log
idea.plugins.path=$SANDBOX/plugins
idea.is.internal=true
PROP
cat > "$SANDBOX/idea.vmoptions" <<'VM'
-Djava.awt.headless=false
-Dide.browser.jcef.headless.enabled=false
-Didea.trust.all.projects=true
-Djb.privacy.policy.text=accepted
-Djb.consents.confirmation.enabled=false
-Didea.show.tips.on.startup.default.value=false
-Xmx1024m
VM
echo 'e2e' > "$SANDBOX/project/README.md"
export IDEA_PROPERTIES="$SANDBOX/idea.properties"
export IDEA_VM_OPTIONS="$SANDBOX/idea.vmoptions"
pkill -9 -f "$IDEA_HOME/bin" 2>/dev/null || true
xvfb-run -a -s "-screen 0 1280x800x24" "$IDEA_HOME/bin/idea.sh" "$SANDBOX/project" \
  >"$EV/idea-stdout.log" 2>&1 &
PID=$!
LOADED=0
for i in $(seq 1 90); do
  if grep -q 'Loaded custom plugins: SHAFT' "$SANDBOX/log/idea.log" 2>/dev/null; then
    LOADED=1; echo "PLUGIN_LOADED_AT_SEC=$i"; break
  fi
  sleep 1
done
sleep 5
kill "$PID" 2>/dev/null || true
sleep 2
pkill -9 -f "$IDEA_HOME/bin" 2>/dev/null || true
grep 'Loaded custom plugins: SHAFT' "$SANDBOX/log/idea.log" | tee "$EV/e2e-plugin-loaded.log"
[[ "$LOADED" -eq 1 ]] || { echo "FAIL: SHAFT plugin did not load"; exit 1; }
echo "SMOKE_OK evidence=$EV"
