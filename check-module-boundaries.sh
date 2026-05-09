#!/usr/bin/env bash
# Fails if Selenium, Appium, or RestAssured appears in shaft-core, shaft-api, or shaft-db
# transitive dependency trees. Run from the repo root after `mvn install -DskipTests`.
set -euo pipefail

echo "▶ Checking shaft-core has no Selenium..."
if mvn dependency:tree -pl shaft-core -q 2>&1 | grep -i "selenium"; then
  echo "ERROR: Selenium found in shaft-core dependency tree" >&2
  exit 1
fi

echo "▶ Checking shaft-api has no Selenium..."
if mvn dependency:tree -pl shaft-api -am -q 2>&1 | grep -i "selenium"; then
  echo "ERROR: Selenium found in shaft-api dependency tree" >&2
  exit 1
fi

echo "▶ Checking shaft-db has no Selenium..."
if mvn dependency:tree -pl shaft-db -am -q 2>&1 | grep -i "selenium"; then
  echo "ERROR: Selenium found in shaft-db dependency tree" >&2
  exit 1
fi

echo "▶ Checking shaft-api has no Appium..."
if mvn dependency:tree -pl shaft-api -am -q 2>&1 | grep -i "appium"; then
  echo "ERROR: Appium found in shaft-api dependency tree" >&2
  exit 1
fi

echo "▶ Checking shaft-db has no RestAssured..."
if mvn dependency:tree -pl shaft-db -am -q 2>&1 | grep -i "rest-assured"; then
  echo "ERROR: RestAssured found in shaft-db dependency tree" >&2
  exit 1
fi

echo "✅ Module boundary check passed"
