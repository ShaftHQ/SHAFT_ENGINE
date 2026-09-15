#!/usr/bin/env bash
# Build and install io.github.shafthq:allure-cli:<version>:zip into the local Maven repo (#5815).
set -euo pipefail
ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
exec mvn -f "${ROOT}/allure-cli/pom.xml" clean install -Dgpg.skip "$@"
