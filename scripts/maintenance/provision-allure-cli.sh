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

pin="$(cd "$(dirname "$0")/../ci/npm-pins/allure-${ALLURE_VERSION}" && pwd)"
if [[ ! -f "${pin}/package-lock.json" ]]; then
  echo "No npm ci pin for allure@${ALLURE_VERSION}. Add scripts/ci/npm-pins/allure-${ALLURE_VERSION}." >&2
  exit 1
fi
echo "Provisioning Allure CLI via npm ci into ${CACHE_HOME}"
cp "${pin}/package.json" "${pin}/package-lock.json" "${CACHE_HOME}/"
npm ci --prefix "${CACHE_HOME}" --ignore-scripts
