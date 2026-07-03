# IntelliJ Plugin Capture Workflow

Use this flow to validate the assistant-driven e2e capture path. This workflow does not include video capture.

## Build

1. Build and verify the latest local IntelliJ plugin:
   - `powershell -NoProfile -ExecutionPolicy Bypass -File tools\intellij-plugin-recording\record-onboarding.ps1`
   - Equivalent: `gradle -p shaft-intellij check buildPlugin verifyPlugin`
   - On Windows, the script creates a missing `%JAVA_HOME%\Packages` directory before Gradle runs and exits with a clear error if `%JAVA_HOME%` is missing, invalid, or unwritable (this avoids local IntelliJ instrumentation failures seen with Microsoft JDK 21).
2. Launch the recording IDE in an isolated Gradle IntelliJ sandbox:
   - `gradle -p shaft-intellij runIde --args <repo-root>`
   - Fallback: install `shaft-intellij/build/distributions/*.zip` into an isolated IntelliJ sandbox and open `Tools -> SHAFT -> Open SHAFT`.

## Default UI

Regular users see only Assistant and configuration. Guided workflows, direct tool panels, provider routing, cloud settings, provider keys, and source mutation controls require the advanced UI feature flag.

The default assistant route is local. The advanced UI flag is disabled by default and must remain opt-in.

## First-Run Setup

1. Open SHAFT.
2. Select `Family: CODEX`.
3. Select `Runtime: CLI`.
4. Click `Install / Update SHAFT MCP`.
5. Click `Test MCP`.
6. Confirm the setup view closes and SHAFT Assistant opens after a successful test.

## Assistant Browser Control

The Assistant accepts deterministic browser-control prompts from chat. WebDriver is the default backend for `/browser`, `/web`, `/browse`, `/page`, `/inspect`, and `/locator`; Playwright is only used when the prompt explicitly includes `playwright` or `pw`.

Examples:

- `/browser open https://example.com sign in` initializes WebDriver with Chrome, opens the target URL, and returns bounded DOM plus locator candidates.
- `open https://example.com and inspect the sign in link` follows the same WebDriver sequence from natural language.
- `/web dom`, `/browse screenshot target/shaft-browser/home.png`, `/browser title`, `/browser url`, `/browser refresh`, `/browser back`, `/browser forward`, `/browser maximize`, `/browser fullscreen`, and `/browser quit` map to the matching WebDriver MCP tools.
- `/browser playwright open https://example.com` initializes Playwright and navigates only because Playwright was explicitly requested.

Screenshot commands write workspace evidence paths and omit base64 payloads by default.

## Capture Scenario

1. Send `start recording` in Assistant.
2. Confirm the managed browser opens and remains open.
3. In the managed browser:
   - open DuckDuckGo
   - search for `SHAFT Engine`
   - open the first result
   - validate the page title
4. Send `stop` in Assistant.
5. Review the generated capture code blocks shown in chat.
6. Approve generation with `approve`, `okay`, or `generate`.
7. Confirm the local agent writes Page Object Model automation files after approval.

## Assistant Mobile Recording

The Assistant exposes the mobile recording path from chat without opening Appium Inspector or any external UI on its own. Recording commands default to workspace-contained JSON paths and keep typed values out of persisted actions unless `includeSensitiveValues` is explicitly enabled in the reviewed MCP arguments.

Examples:

- `/mobile-record start recordings/mobile.json` starts an MCP mobile action recording.
- `/app-record stop` stops the active recording without discarding it; use a reviewed `discard=true` argument only when the recording should be deleted.
- `/mobile-codegen recordings/mobile.json` and `/mobile-replay recordings/mobile.json` render reusable SHAFT mobile replay snippets from the saved recording path.
- `/mobile-record inspector Android recordings/inspector.json` prepares a confirmation-ready wrapped Appium Inspector recording plan. The returned confirmation token must be passed to `mobile_inspector_record_start` before Inspector can run.
- `/inspector-record Android recordings/inspector.json` is an alias for Inspector preparation, not an implicit request to open a browser or desktop Inspector window.

Expected chat output:

- Mobile recording status is summarized with active/stopped state, action count, output path, and whether sensitive values are excluded.
- Inspector preparation shows readiness, confirmation requirement, device/AVD selection, output path, next steps, warnings, and code blocks without exposing raw capability JSON unless the user asks for raw MCP output.
- Inspector status shows active/paused state, Appium/Inspector URLs, output path, action count, warnings, and generated replay snippets after stop.

## Expected Behavior

- `start recording` uses a fresh capture output path for each session.
- `stop` triggers review-only code block generation.
- File generation waits for approval.
- Existing valid capture output is cleaned before reuse; invalid output fails with a clear error.
- Chat bubbles have no speaker labels.
- Chat session titles are derived from prompt content without speaker labels or local account names.
