#!/usr/bin/env bash
# Build and install io.github.shafthq:allure-cli:<version>:zip into the local Maven repo (#5815).
# Operator docs: chaos-engine/profiles/shaft/references/shaft-mastery/allure-internals.md
set -euo pipefail
ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
exec mvn -f "${ROOT}/allure-cli/pom.xml" clean install -Dgpg.skip "$@"
