#!/usr/bin/env bash
# Prefer Maven zip unpack into the AllureManager runtime cache; fall back to npm (#5801/#5815).
set -euo pipefail

CACHE_HOME="${1:?cache home}"
MAVEN_ZIP="${2:?maven zip path}"
ALLURE_VERSION="${3:?allure version}"
CLI_JS="${CACHE_HOME}/node_modules/allure/cli.js"

mkdir -p "${CACHE_HOME}"

if [[ -f "${CLI_JS}" ]]; then
  echo "Allure CLI already present: ${CLI_JS}"
  exit 0
fi

if [[ -f "${MAVEN_ZIP}" ]]; then
  echo "Unpacking Maven Allure CLI zip: ${MAVEN_ZIP} -> ${CACHE_HOME}"
  python3 - "${MAVEN_ZIP}" "${CACHE_HOME}" <<'PY'
import sys
import zipfile
from pathlib import Path

zip_path = Path(sys.argv[1])
dest = Path(sys.argv[2])
with zipfile.ZipFile(zip_path) as zf:
    zf.extractall(dest)
PY
  if [[ -f "${CLI_JS}" ]]; then
    echo "Provisioned Allure CLI from Maven zip: ${CLI_JS}"
    exit 0
  fi
  echo "Maven zip unpacked but cli.js missing at ${CLI_JS}; falling back to npm"
else
  echo "Maven zip not found at ${MAVEN_ZIP}; falling back to npm"
fi

printf '%s\n' '{' '  "name": "shaft-allure-cli-runtime",' '  "private": true' '}' > "${CACHE_HOME}/package.json"
echo "Provisioning Allure CLI via npm into ${CACHE_HOME}"
npm --prefix "${CACHE_HOME}" install --no-package-lock --no-save --ignore-scripts "allure@${ALLURE_VERSION}"
