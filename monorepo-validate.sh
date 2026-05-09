#!/usr/bin/env bash
# Runs each module in isolation to prove the dependency chain holds independently.
# Run from the repo root. Requires Maven and JDK 25.
set -euo pipefail

echo "▶ Installing all modules to local .m2 (skip tests)..."
mvn install -DskipTests -q

echo "▶ shaft-core (isolated)"
mvn test -pl shaft-core -q

echo "▶ shaft-api (isolated — resolves shaft-core from local .m2)"
mvn test -pl shaft-api -am -q

echo "▶ shaft-db (isolated — resolves shaft-core from local .m2)"
mvn test -pl shaft-db -am -q

echo "▶ shaft-web (full stack)"
mvn test -pl shaft-web -am -q

echo "▶ Module boundary check"
bash check-module-boundaries.sh

echo "✅ All modules passed"
