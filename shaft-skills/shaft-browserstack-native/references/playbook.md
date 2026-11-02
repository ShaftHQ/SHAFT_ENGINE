# BrowserStack native playbook

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
- Map / confirm taps that stay green with no UI change: consumer triage in `shaft-mobile-retest-hygiene`; engine observability in SHAFT #7706.

## Boundary

- Gate IPA / APK artifacts before upload via the companion skills.
- No credentials, tokens, or client product names in commits or skill text.
