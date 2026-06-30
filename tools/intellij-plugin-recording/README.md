# IntelliJ Plugin Capture Workflow

Use this flow to validate the assistant-driven e2e capture path. This workflow does not include video capture.

## Build

1. Build and verify the latest local IntelliJ plugin:
   - `powershell -NoProfile -ExecutionPolicy Bypass -File tools\intellij-plugin-recording\record-onboarding.ps1`
   - Equivalent: `gradle -p shaft-intellij check buildPlugin verifyPlugin`
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

## Expected Behavior

- `start recording` uses a fresh capture output path for each session.
- `stop` triggers review-only code block generation.
- File generation waits for approval.
- Existing valid capture output is cleaned before reuse; invalid output fails with a clear error.
- Chat bubbles have no speaker labels.
- Chat session titles are derived from prompt content without speaker labels or local account names.
