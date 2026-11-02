#!/usr/bin/env bash
# Host checks for Ubuntu_Flutter_Emulator_Local. Must run before emulator-runner.
# sdkmanager can prompt for the Android XR (extended reality) preview license even
# when the AVD is a phone image; accept licenses non-interactively or the later
# emulator-runner sdk step hangs.
set -eu

if [ ! -e /dev/kvm ]; then
  echo "::error::KVM device /dev/kvm is missing; Flutter emulator E2E requires hardware virtualization."
  exit 1
fi
if [ ! -r /dev/kvm ] || [ ! -w /dev/kvm ]; then
  echo 'KERNEL=="kvm", GROUP="kvm", MODE="0666", OPTIONS+="static_node=kvm"' | sudo tee /etc/udev/rules.d/99-kvm4all.rules >/dev/null
  sudo udevadm control --reload-rules
  sudo udevadm trigger --name-match=kvm
  sudo chmod 666 /dev/kvm || true
fi
if [ ! -r /dev/kvm ] || [ ! -w /dev/kvm ]; then
  echo "::error::KVM device /dev/kvm is not read/write for the runner user."
  exit 1
fi

if ! command -v sdkmanager >/dev/null 2>&1; then
  echo "::error::sdkmanager is not on PATH; Android SDK must be provisioned before Flutter emulator E2E."
  exit 1
fi

yes | sdkmanager --licenses >/dev/null || true
# Android XR preview packages ship a distinct license id; refusing it later
# aborts google_apis system-image downloads used by emulator-runner.
yes | sdkmanager --licenses >/dev/null || true

if ! command -v adb >/dev/null 2>&1; then
  echo "::error::adb is not on PATH after Android SDK setup."
  exit 1
fi
adb start-server
adb version

# Pin lives under scripts/ci/npm-pins. Same-shell `export PATH` is not enough
# for later Actions steps (android-emulator-runner): persist via $GITHUB_PATH.
pin="$(cd "$(dirname "$0")/npm-pins/appium-3.5.2" && pwd)"
pin_bin="$pin/node_modules/.bin"
if ! command -v appium >/dev/null 2>&1; then
  npm ci --prefix "$pin"
  if [ ! -e "$pin_bin/appium" ]; then
    echo "::error::Pinned Appium binary missing at $pin_bin/appium after npm ci."
    exit 1
  fi
fi
if [ -e "$pin_bin/appium" ]; then
  export PATH="$pin_bin:$PATH"
  if [ -n "${GITHUB_PATH:-}" ]; then
    echo "$pin_bin" >> "$GITHUB_PATH"
  fi
fi
if ! command -v appium >/dev/null 2>&1; then
  echo "::error::appium not on PATH after Flutter emulator preflight."
  exit 1
fi
appium driver install --source npm appium-flutter-integration-driver

echo "Flutter emulator preflight OK."
