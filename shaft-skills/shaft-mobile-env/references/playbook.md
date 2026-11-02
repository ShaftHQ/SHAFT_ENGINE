# Mobile-env playbook

Source lessons from a client mobile project (abstracted). No secrets or hostnames.

## JDK split (FR-1)

- Maven / SHAFT tests compiling with `--release 25` need Temurin **25** as `JAVA_HOME`.
- Android app Gradle builds often still need Temurin **21** with a full `javac` (JRE-only JDKs fail the Android toolchain).
- Symptom of the wrong Maven JDK: `class file version 69` / unsupported class file.
- Keep two JDKs installed; switch `JAVA_HOME` per path (tests vs app build). Never point the Android Gradle path at a JRE-only 25.

## Appium pin (FR-2)

- Prefer versions that match SHAFT's `MOBILE_ANDROID` catalog (known-good example: Appium **3.8.0**, uiautomator2 **8.5.2`).
- Prefer the SHAFT-managed Appium under `~/.local/share/shafthq/*/tools/appium/<version>` over a random global install.
- Start once with `--relaxed-security --base-path / --port 4723`. Reuse one process.
- Confirm `GET /status` before Maven. Do not start a second Appium beside a healthy one this agent owns.

## Android SDK / AVD (FR-3)

- Set both `ANDROID_HOME` and `ANDROID_SDK_ROOT` to the same root.
- Put `platform-tools`, `emulator`, and `cmdline-tools/latest/bin` on `PATH`.
- Prefer `shaft-cli setup doctor|plan|install --profile MOBILE_ANDROID` before manual SDK work.
- If the installer pins a dead emulator build, fall back to manual packages and file upstream.
- When screenshots matter, match AVD geometry to the cloud device under test.

## Emulator memory (FR-4)

- Headless qemu host RSS can grow far beyond guest RAM and OOM the desktop.
- On systemd user systems: `systemd-run --user --scope -p MemoryMax=10G -p MemorySwapMax=0` around the emulator start.
- Cold-boot (`-no-snapshot`) when dead, offline, or snapshot mismatch — not between every test method.
- Non-systemd hosts: document an equivalent cgroup/ulimit and refuse unbounded qemu.

## Session repair checklist (FR-5)

1. `shaft-cli setup doctor --profile MOBILE_ANDROID` (or project `env-check`).
2. Fix only reported gaps (JDK major, SDK vars, Appium `/status`, AVD alive).
3. Re-run doctor until clean.
4. One smoke method with retry 0 on the owned session.
5. Stop the Appium this agent started when the suite ends.

## Boundary

- BrowserStack cloud setup → `shaft-browserstack-native`.
- IPA / APK selection → `shaft-ios-ipa-acceptance` / `shaft-android-apk-select`.
- Do not install paid cloud capacity or mutate shared infrastructure without approval.
