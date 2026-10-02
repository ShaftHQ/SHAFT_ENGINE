# Appium & Mobile Toolchains

## Stack shape
Appium 2.x = server + installable drivers (`uiautomator2`, `xcuitest`,
`windows`) + plugins, each versioned independently — pin them in CI, `appium
driver install` at run time is a supply-chain and race liability. SHAFT talks
to it via `mobile_*` MCP tools and the shaft-mcp mobile toolchain service;
web emulation (Chrome device metrics) is a separate, cheaper path from real
native automation.

## Incidents that must not repeat
- Never use `appium driver run windows install-wad` in CI: it lists
  `api.github.com/repos/microsoft/winappdriver/releases` unauthenticated and
  403s under shared Actions IP rate limits (#5697). Install a pinned MSI via
  `scripts/ci/install_winappdriver.ps1` (or an equivalent authenticated /
  cached asset path). The older race where `install-wad` returned before the
  MSI finished still applies if you resurrect that path — poll for
  `WinAppDriver.exe` before starting Appium (PR #3408).
- Chrome's emulated-device list drifts: Chrome 143 removed "Pixel 5",
  silently breaking every mobile-web-emulation flow pinned to it. Treat
  device names as data that rots; validate against the running browser and
  keep a fallback metrics map (PR #3421).
- Keep the server log retrievable on failure: in PowerShell CI, `Receive-Job`
  BEFORE `Stop-Job`, or the diagnosis evidence dies with the job.

## Context switching
Native automation lives in `NATIVE_APP` context; webviews appear as separate
contexts needing matching chromedriver versions. `mobile_get_contexts` before
switching; hybrid-app flakiness is usually a context (not locator) problem.
Accessibility-tree reads (`mobile_get_accessibility_tree`) beat raw XML
source dumps for both tokens and stability.

## CI reality
- Android emulators in CI need: KVM/HAXM availability check, boot-completed
  polling (`sys.boot_completed`), and generous first-boot timeouts; never
  fixed sleeps.
- iOS/Safari and device-cloud (BrowserStack) failures are frequently
  provider-side or target-site-side: before touching code, curl the target
  page and check the same test's pass history — a locator that passed 6 of 7
  scheduled runs and the element still existing live means external incident,
  not regression (PR #3408 triage).
