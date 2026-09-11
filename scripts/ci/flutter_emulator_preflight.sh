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

if ! command -v appium >/dev/null 2>&1; then
  npm install -g appium@3.5.2
fi
appium driver install --source npm appium-flutter-integration-driver

echo "Flutter emulator preflight OK."
