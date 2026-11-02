# Android APK select playbook

## Decision table (FR-1)

| Target | Prefer | Fallback |
| --- | --- | --- |
| Local x86_64 emulator (`ro.product.cpu.abi` = x86_64) | universal APK, or x86_64 split | never arm64-only |
| Real arm64 device / BrowserStack Android | arm64-v8a split (smaller upload) | universal on ABI install errors |

```bash
python3 scripts/mobile/android_apk_select.py \
  --abi x86_64 \
  --apk app-universal.apk --kind universal \
  --apk app-arm64.apk --kind arm64-v8a
```

## Install hygiene (FR-2)

- Prefer a fresh install before a suite when version expectations changed.
- Appium may skip reinstall when `versionCode` is unchanged, leaving mixed expectations — force reinstall (`adb install -r -d` locally, or the project's Appium equivalent) when the binary under test changed.
- Cloud parity: device control goes through Appium, not raw `adb`, when the suite must match BrowserStack.

## Flavor ↔ backend matrix (FR-3)

Every consumer README must document a table:

| Build flavor | Backend host class | Automation tags / a11y ids | Notes |
| --- | --- | --- | --- |
| debug / automation | non-prod | present | preferred for automation |
| release | prod | often stripped | refuse for automation |

Never automate against a release flavor that strips content-descriptions / accessibility ids. Request the debug/automation flavor instead.

## Boundary

- No client hostnames in this skill; use "non-prod" / "prod" classes only.
