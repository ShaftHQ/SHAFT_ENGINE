# Mobile test app fixtures

The apk/ipa binaries used by mobile E2E tests are **not** stored in git
(see [#7547](https://github.com/ShaftHQ/SHAFT_ENGINE/issues/7547)).

Pinned release: [`test-fixtures-mobile-apps-v1`](https://github.com/ShaftHQ/SHAFT_ENGINE/releases/tag/test-fixtures-mobile-apps-v1).

Fetch (and cache) them into this directory:

```bash
python3 scripts/ci/fetch_mobile_test_apps.py
```

Manifest: [`scripts/ci/mobile_test_apps.manifest.json`](../../../../../../scripts/ci/mobile_test_apps.manifest.json).

BrowserStack iOS jobs overwrite `BStackSampleApp.ipa` with the
provider-compatible sample from `browserstack/testng-appium-app-browserstack`
after this fetch. Flutter CI jobs may replace `flutter-demo.apk` with a freshly
built `app-debug.apk`.
