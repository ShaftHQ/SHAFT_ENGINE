# Mobile runtime

Use when setting up or repairing a local SHAFT mobile env (JDK split, Appium pin, SDK/AVD, emulator memory, or the doctor loop), choosing a universal or ABI-split APK, gating an iOS IPA before cloud upload, or configuring BrowserStack App Automate.

Emulator cold-boot, memory pressure, and the one-device Appium rule: [mobile session policy](../mobile-session-policy.md).

Source lessons from a client mobile project (abstracted). No secrets or hostnames.

## Local environment

## JDK split (FR-1)

- Maven / SHAFT tests compiling with `--release 25` need Temurin **25** as `JAVA_HOME`.
- Android app Gradle builds often still need Temurin **21** with a full `javac` (JRE-only JDKs fail the Android toolchain).
- Symptom of the wrong Maven JDK: `class file version 69` / unsupported class file.
- Keep two JDKs installed; switch `JAVA_HOME` per path (tests vs app build). Never point the Android Gradle path at a JRE-only 25.

## Appium pin (FR-2)

- Prefer versions that match SHAFT's `MOBILE_ANDROID` catalog (known-good example: Appium **3.8.0**, uiautomator2 **8.5.2`).
- Prefer the SHAFT-managed Appium under `~/.local/share/shafthq/*/tools/appium/<version>` over a random global install.
- Start flags, one process, and `GET /status` are in [the mobile session policy](../mobile-session-policy.md).

## Android SDK / AVD (FR-3)

- Set both `ANDROID_HOME` and `ANDROID_SDK_ROOT` to the same root.
- Put `platform-tools`, `emulator`, and `cmdline-tools/latest/bin` on `PATH`.
- Prefer `shaft-cli setup doctor|plan|install --profile MOBILE_ANDROID` before manual SDK work.
- If the installer pins a dead emulator build, fall back to manual packages and file upstream.
- When screenshots matter, match AVD geometry to the cloud device under test.

## Emulator memory (FR-4)

Headless qemu RSS, the managed 8 GiB cap, the manual `MemoryMax=10G` scope, and cold-boot flags are in [the mobile session policy](../mobile-session-policy.md).

## Session repair checklist (FR-5)

1. `shaft-cli setup doctor --profile MOBILE_ANDROID` (or project `env-check`).
2. Fix only reported gaps (JDK major, SDK vars, Appium `/status`, AVD alive).
3. Re-run doctor until clean.
4. One smoke method with retry 0 on the owned session.
5. Stop the Appium this agent started when the suite ends. The stop rule is in [the mobile session policy](../mobile-session-policy.md).

## Valid examples

- On a fresh Linux host, install JDK + Android SDK + Appium and confirm `adb devices` lists an emulator before starting a SHAFT mobile suite.
- Repair a wedged local Android emulator by recycling the managed Appium/emulator lease once, then re-running the suite without starting a second Appium beside a live one.

## Boundary

- BrowserStack, IPA acceptance, and APK selection are the sections below.
- Do not install paid cloud capacity or mutate shared infrastructure without approval.

## Android APK selection

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

## Valid examples

- Local x86_64 emulator: pass both universal and arm64-v8a APKs to `android_apk_select.py --abi x86_64` and install the universal (or x86_64) artifact, never the arm64-only split.
- BrowserStack Android arm64 device: prefer the arm64-v8a split for a smaller upload; fall back to universal only when the ABI install fails.

## Boundary

- No client hostnames in this skill; use "non-prod" / "prod" classes only.

## iOS IPA acceptance

Fail closed before burning cloud minutes on a silently-wrong artifact.

## Checklist (FR-1)

1. **Real device only** — App Automate does not run simulators. Simulator builds can upload quietly then sit on SpringBoard.
2. **Platform** — `DTPlatformName` must be `iphoneos`. Reject `iphonesimulator`.
3. **Signing** — Development / Ad Hoc / Enterprise for Automate. Refuse App Store / TestFlight-only packages when Automate cannot install them.
4. **Architecture** — arm64. Document thinning: prefer none (or a thinning that still yields a device IPA).
5. **Environment match** — Ask app owners for the scheme/environment that matches the backend under test, and for debug flags needed for session-id login / certificate unpin if Local is required.

## Scripted probe (FR-2)

```bash
python3 scripts/mobile/ios_ipa_acceptance.py --ipa path/to/app.ipa
# or against an already-extracted Info.plist:
python3 scripts/mobile/ios_ipa_acceptance.py --plist path/to/Info.plist
```

Exit non-zero when `DTPlatformName` is missing or not `iphoneos`. Print a short checklist; never dump IPA contents or client identifiers.

Manual interim: unzip the IPA, locate `Payload/*.app/Info.plist`, convert with `plutil -convert xml1` if binary, then read `DTPlatformName`.

## Consumer swipe note

iOS SwiftUI swipe may need a tallest `XCUIElementTypeScrollView` + `mobile: dragFromToForDuration` fallback until the weekly release that ships #7680 / PR #7681 is on the pom; then drop the fallback. See [mobile retest hygiene](mobile-retest-hygiene.md).

## Valid examples

- Refuse upload of an unsigned or expired-provisioning IPA and print the gate reason before any BrowserStack App Automate call.
- Accept a debug IPA that passes the embedded.mobileprovision expiry and bundle-id checks, then hand the path to the BrowserStack upload step.

## Boundary

- English-only public text; no IPA contents or client identifiers required to follow this skill.

## BrowserStack App Automate

Abstracted for any SHAFT consumer. Never print secret values.

## Secrets (FR-1)

Required property **key names** (values only in a gitignored properties file):

- `browserStack.userName`
- `browserStack.accessKey`
- `browserStack.appUrl`
- optional `browserStack.local`

Rules:

- Example files list **key names only**.
- Secret-scan before every commit; never `--no-verify`.
- Allure single-file HTML and `browserstack.yml` can embed keys — keep them out of git.
- Refuse to echo values when keys are missing; name the keys and stop.

## Device pin (FR-2)

- Pin exact `deviceName` + `platformVersion` pairs.
- Validate against `/app-automate/devices.json` before a long queue.
- Do not invent device names from memory.

## App identity (FR-3)

- Prefer immutable `bs://` app ids over moving `custom_id` aliases when a fixed build under test is required.
- Uploads expire after idle time; re-list `/app-automate/recent_apps` before long reruns.
- Record the chosen `bs://` id in the run receipt.

## Shared build naming (FR-4)

- Native SHAFT drivers should share one BrowserStack project/build name helper so sessions do not fragment by class or date.
- Use the project's helper (or one `browserStack.buildName` / project name) for the whole suite.

## Local + triage (FR-5)

- Enable `browserStack.local` only when the backend requires a tunnel.
- Force-update / blank launch on cloud devices is often a **backend gate**, not a locator bug — check device logs first.
- Document any unpin / session-id debug flags the app must expose; do not invent them.
- Map / confirm taps that stay green with no UI change: consumer triage in [mobile retest hygiene](mobile-retest-hygiene.md); engine observability in SHAFT #7706.

## Valid examples

- Upload a debug Android APK and start an App Automate session with the project's non-prod capabilities, never a release build that strips content-descriptions.
- After a failed iOS cloud run, download the App Automate session video and device logs before retrying so the next attempt can cite evidence instead of guessing.

## Boundary

- Gate IPA / APK artifacts before upload via the companion skills.
- No credentials, tokens, or client product names in commits or skill text.
