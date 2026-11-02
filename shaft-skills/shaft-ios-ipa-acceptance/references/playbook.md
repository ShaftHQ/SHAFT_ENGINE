# iOS IPA acceptance playbook

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

iOS SwiftUI swipe may need a tallest `XCUIElementTypeScrollView` + `mobile: dragFromToForDuration` fallback until the weekly release that ships #7680 / PR #7681 is on the pom; then drop the fallback. See `shaft-mobile-retest-hygiene`.

## Valid examples

- Refuse upload of an unsigned or expired-provisioning IPA and print the gate reason before any BrowserStack App Automate call.
- Accept a debug IPA that passes the embedded.mobileprovision expiry and bundle-id checks, then hand the path to the BrowserStack upload step.

## Boundary

- English-only public text; no IPA contents or client identifiers required to follow this skill.
