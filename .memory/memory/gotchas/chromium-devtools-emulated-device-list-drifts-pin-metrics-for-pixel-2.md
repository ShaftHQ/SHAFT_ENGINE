# Chromium DevTools emulated-device list drifts; pin metrics for Pixel 2/5

Chrome periodically drops named devices (e.g. "Pixel 2", historically "Pixel 5")
from its DevTools list. Passing bare `mobileEmulation.deviceName` then fails session
creation with `must be a valid device` / `firstMatch is invalid` (#6133
MacOSX_Chrome_Local). SHAFT pins metrics+UA in `EmulatedDeviceProfiles` so OptionsManager
never depends on the browser's live device catalog for those names.

Related evidence-path hardening on the same nightlies: ScreenshotHelper must not
`(JavascriptExecutor)`-cast plain Mockito WebDrivers; ImageProcessingActions highlight
must soft-fail on undecodable stub bytes; PlaywrightValidationsExecutor must skip
screenshot/HTML attach when evidence Page is null (#6132/#6133 fingerprints).
