# SHAFT Modular-Era Feature Catalog

SHAFT is now a modular Java 25 automation framework with a lean core, opt-in power modules, and evidence-first tooling for browser, mobile, API, Playwright, MCP, IntelliJ IDEA, SikuliX desktop image automation, capture, reporting, diagnosis, and healing.

This catalog is written for framework users who want to know what changed, what they can adopt now, and which command or API gets them started.

- Baseline: `35d51c56289af07a4204cc52d2ee30e55be172e3` (`Shaft modularization (#2839)`)
- Catalog source: current PR branch based on `origin/main` at `465fb54942`
- Fresh evidence captured: `2026-07-04`

## Start Here

| If you need to... | Start with... | Why it matters |
| --- | --- | --- |
| Upgrade without pulling every optional integration into the core artifact. | [Modular adoption](#modular-adoption) | `shaft-engine` stays lean while BrowserStack, visual, video, AI, Doctor, Heal, and Capture remain available as opt-in modules. |
| Let an agent inspect pages, choose locators, record flows, and review generated code. | [MCP and agent workflows](#mcp-and-agent-workflows) | `shaft-mcp` exposes WebDriver, Playwright, mobile, capture, Doctor, Heal, Trace, guide search, and guardrail tools through one automation surface. |
| Use SHAFT workflows inside IntelliJ IDEA. | [IntelliJ IDEA plugin](#intellij-idea-plugin) | The stable plugin is the front door for coding-partner work: Assistant and Guided collect intent, current source, selected text, and evidence before MCP plans reuse and verification. |
| Turn exploratory browser or mobile sessions into maintainable Java tests. | [Capture and code generation](#capture-and-code-generation) | Recorder sessions preserve actions, checkpoints, locators, context, privacy, and replay snippets. |
| Make Android/Appium setup and recording less coordinate-driven. | [Mobile automation](#mobile-automation) | Toolchain diagnostics and locator-first Inspector recording show the exact device, locator, and fallback state. |
| Debug failed tests from evidence instead of guessing. | [Doctor, Heal, Trace, and reporting](#doctor-heal-trace-and-reporting) | Failure briefs, traces, locator health, healing decisions, and report UI give a shorter path from failure to fix. |

### IntelliJ Assistant browser control

Assistant chat now routes browser-control requests directly to sequenced SHAFT MCP tools. WebDriver is the default for natural prompts such as `open https://example.com and inspect the sign in link`. The generated sequence calls `driver_initialize` before `browser_open_intent`, keeps DOM output bounded, and returns locator candidates with evidence.

Browser-control requests can inspect DOM, title, URL, screenshots, navigation, window state, and session cleanup. Screenshot requests write workspace evidence files and omit base64 by default. Playwright remains available only through explicit wording in natural prompts.

## Screenshot Proof Gallery

Every screenshot in this catalog is real repository evidence under `shaft-engine/src/main/resources/modular-era-feature-catalog/`.

### IntelliJ plugin screenshots

- `shaft-engine/src/main/resources/modular-era-feature-catalog/intellij-plugin-assistant.png`
- `shaft-engine/src/main/resources/modular-era-feature-catalog/intellij-plugin-assistant-empty.png`
- `shaft-engine/src/main/resources/modular-era-feature-catalog/intellij-plugin-assistant-live-output-dark.png`
- `shaft-engine/src/main/resources/modular-era-feature-catalog/intellij-plugin-assistant-narrow-dark.png`
- `shaft-engine/src/main/resources/modular-era-feature-catalog/intellij-plugin-mcp-setup.png`
- `shaft-engine/src/main/resources/modular-era-feature-catalog/intellij-plugin-mcp-setup-narrow-dark.png`
- `shaft-engine/src/main/resources/modular-era-feature-catalog/intellij-plugin-mcp-setup-success.png`
- `shaft-engine/src/main/resources/modular-era-feature-catalog/intellij-plugin-mcp-setup-error-dark.png`
- `shaft-engine/src/main/resources/modular-era-feature-catalog/intellij-plugin-settings.png`

<table>
  <tr>
    <td width="50%">
      <img src="shaft-engine/src/main/resources/modular-era-feature-catalog/intellij-plugin-assistant.png" alt="SHAFT IntelliJ IDEA plugin Assistant tab" width="620">
      <br><strong>IntelliJ Assistant</strong>
      <br>Copilot-style composer with visible Ask/Plan/Agent mode switching, Local/Cloud provider type, family/runtime, prompt, and transcript.
    </td>
    <td width="50%">
      <img src="shaft-engine/src/main/resources/modular-era-feature-catalog/intellij-plugin-assistant-dark.png" alt="SHAFT IntelliJ IDEA plugin Assistant tab in dark theme" width="620">
      <br><strong>Assistant (dark)</strong>
      <br>Assistant workflow view in IntelliJ dark theme for the same prompt/action path.
    </td>
  </tr>
  <tr>
    <td width="50%">
      <img src="shaft-engine/src/main/resources/modular-era-feature-catalog/intellij-plugin-assistant-live-output-dark.png" alt="SHAFT IntelliJ IDEA plugin Assistant tab while a local agent is running" width="620">
      <br><strong>Assistant live output</strong>
      <br>Local agent execution keeps transcript output, progress, cancellation, and copy controls visible in the tool window.
    </td>
    <td width="50%">
      <img src="shaft-engine/src/main/resources/modular-era-feature-catalog/intellij-plugin-assistant-narrow-dark.png" alt="SHAFT IntelliJ IDEA plugin Assistant tab in a narrow dark tool window" width="620">
      <br><strong>Assistant narrow panel</strong>
      <br>The same Assistant controls stay usable in a narrow right-side IntelliJ tool window.
    </td>
  </tr>
  <tr>
    <td width="50%">
      <img src="shaft-engine/src/main/resources/modular-era-feature-catalog/intellij-plugin-guided.png" alt="SHAFT IntelliJ IDEA plugin Guided tab" width="620">
      <br><strong>Guided workflows</strong>
      <br>Recorder, locator, and guardrail actions prepare reviewed SHAFT MCP requests.
    </td>
    <td width="50%">
      <img src="shaft-engine/src/main/resources/modular-era-feature-catalog/intellij-plugin-triage.png" alt="SHAFT IntelliJ IDEA plugin Triage tab" width="620">
      <br><strong>Triage</strong>
      <br>Doctor, Trace, Healer, and locator proposal requests are prepared from one evidence workflow.
    </td>
  </tr>
  <tr>
    <td width="50%">
      <img src="shaft-engine/src/main/resources/modular-era-feature-catalog/intellij-plugin-recorder.png" alt="SHAFT IntelliJ IDEA plugin Recorder tab" width="620">
      <br><strong>Recorder</strong>
      <br>Editable capture templates, checkpoints, Playwright/native replay settings, and request/result state.
    </td>
    <td width="50%">
      <img src="shaft-engine/src/main/resources/modular-era-feature-catalog/intellij-plugin-inspector.png" alt="SHAFT IntelliJ IDEA plugin Inspector tab" width="620">
      <br><strong>Inspector</strong>
      <br>Inspector-backed status and MCP request inspection during capture and mobile tool workflows.
    </td>
  </tr>
  <tr>
    <td width="50%">
      <img src="shaft-engine/src/main/resources/modular-era-feature-catalog/intellij-plugin-evidence.png" alt="SHAFT IntelliJ IDEA plugin Evidence Tools tab" width="620">
      <br><strong>Evidence Tools</strong>
      <br>Evidence and repair-oriented outputs, including locator and failure-context helpers.
    </td>
    <td width="50%">
      <img src="shaft-engine/src/main/resources/modular-era-feature-catalog/intellij-plugin-projects.png" alt="SHAFT IntelliJ IDEA plugin Projects tab" width="620">
      <br><strong>Projects</strong>
      <br>Project and upgrade actions, migration checkers, and project tool execution surfaces.
    </td>
  </tr>
  <tr>
    <td width="50%">
      <img src="shaft-engine/src/main/resources/modular-era-feature-catalog/intellij-plugin-advanced-tools.png" alt="SHAFT IntelliJ IDEA plugin Advanced Tools tab" width="620">
      <br><strong>Advanced Tools (light)</strong>
      <br>Curated MCP template families and lower-frequency operational tool access.
    </td>
    <td width="50%">
      <img src="shaft-engine/src/main/resources/modular-era-feature-catalog/intellij-plugin-advanced-tools-dark.png" alt="SHAFT IntelliJ IDEA plugin Advanced Tools tab in dark theme" width="620">
      <br><strong>Advanced Tools (dark)</strong>
      <br>Same curated MCP families in IntelliJ dark theme.
    </td>
  </tr>
  <tr>
    <td width="50%">
      <img src="shaft-engine/src/main/resources/modular-era-feature-catalog/intellij-plugin-mcp-setup.png" alt="SHAFT IntelliJ IDEA plugin first-run MCP setup flow" width="620">
      <br><strong>First-run MCP setup</strong>
      <br>A simple click-through workflow highlights one action at a time: pick agent, copy command, open terminal, then check setup.
    </td>
    <td width="50%">
      <img src="shaft-engine/src/main/resources/modular-era-feature-catalog/intellij-plugin-settings.png" alt="SHAFT IntelliJ IDEA plugin Settings panel with MCP and provider controls" width="620">
      <br><strong>Settings and providers</strong>
      <br>Post-setup controls for Local/Cloud routing, cloud model selection, selected-key passing, and provider key storage.
    </td>
  </tr>
  <tr>
    <td width="50%">
      <img src="shaft-engine/src/main/resources/modular-era-feature-catalog/intellij-plugin-mcp-setup-success.png" alt="SHAFT IntelliJ IDEA plugin MCP setup success flow" width="620">
      <br><strong>MCP setup success</strong>
      <br>Successful setup stays clean: SHAFT verifies the runtime, hides managed MCP wiring, and reveals Start chatting.
    </td>
    <td width="50%">
      <img src="shaft-engine/src/main/resources/modular-era-feature-catalog/intellij-plugin-mcp-setup-error-dark.png" alt="SHAFT IntelliJ IDEA plugin MCP setup error state in dark theme" width="620">
      <br><strong>MCP setup error</strong>
      <br>Probe failures stay inline with categorized diagnostics, client-specific next steps, retry actions, and copyable diagnostic output.
    </td>
  </tr>
  <tr>
    <td width="50%">
      <img src="shaft-engine/src/main/resources/modular-era-feature-catalog/intellij-plugin-mcp-setup-narrow-dark.png" alt="SHAFT IntelliJ IDEA plugin MCP setup flow in a narrow dark tool window" width="620">
      <br><strong>MCP setup narrow panel</strong>
      <br>Status labels and runtime controls remain readable in a 360 px right-side tool window.
    </td>
    <td width="50%">
      <img src="shaft-engine/src/main/resources/modular-era-feature-catalog/intellij-plugin-settings-dark.png" alt="SHAFT IntelliJ IDEA plugin Settings panel in dark theme" width="620">
      <br><strong>Settings (dark)</strong>
      <br>Provider routing, selected-key passing, and stored-key controls remain readable in IntelliJ dark theme.
    </td>
  </tr>
  <tr>
    <td width="50%">
      <img src="shaft-engine/src/main/resources/modular-era-feature-catalog/android-recorder-working.png" alt="Android recorder resolving gestures to accessibility id and resource id locators" width="620">
      <br><strong>Android recorder with real locators</strong>
      <br>Inspector gestures are resolved to `ACCESSIBILITY_ID` and `ID` locators before falling back to coordinates.
    </td>
    <td width="50%">
      <img src="shaft-engine/src/main/resources/modular-era-feature-catalog/android-recorder-locator-details.png" alt="Mobile Inspector locator details and coordinate fallback warning" width="620">
      <br><strong>Locator details and fallback warning</strong>
      <br>Shows the locator order, recorded action details, and warning-only coordinate fallback path.
    </td>
  </tr>
  <tr>
    <td width="50%">
      <img src="shaft-engine/src/main/resources/modular-era-feature-catalog/android-toolchain-status.png" alt="Android Appium toolchain status after setup" width="620">
      <br><strong>Android toolchain status</strong>
      <br>Confirms SDK, emulator, Appium, UiAutomator2, Inspector plugin, and readiness checks from one diagnostic surface.
    </td>
    <td width="50%">
      <img src="shaft-engine/src/main/resources/modular-era-feature-catalog/android-emulator-device.png" alt="Android emulator device used for evidence capture" width="620">
      <br><strong>Fresh emulator evidence</strong>
      <br>Shows the real Android emulator target used while validating the mobile recorder path.
    </td>
  </tr>
  <tr>
    <td width="50%">
      <img src="shaft-engine/src/main/resources/modular-era-feature-catalog/mcp-tools.png" alt="shaft-mcp tool manifest and tool families" width="620">
      <br><strong>MCP tool manifest</strong>
      <br>WebDriver, Playwright, mobile, capture, target discovery, backend comparison, evidence packs, Doctor, Heal, Trace, guide search, scenario catalog, and guardrails in one manifest.
    </td>
    <td width="50%">
      <img src="shaft-engine/src/main/resources/modular-era-feature-catalog/capture-catalog.png" alt="Capture and code generation feature catalog" width="620">
      <br><strong>Capture/codegen capability map</strong>
      <br>Session storage, in-panel checkpoints, step reorder, session goals, replay generation, privacy, locator fallback, patch previews, backend comparison, evidence manifests, workbench review sections, and Page Object drafts are visible in source.
    </td>
  </tr>
  <tr>
    <td width="50%">
      <img src="shaft-engine/src/main/resources/modular-era-feature-catalog/playwright-surface.png" alt="Playwright facade and MCP tool surface" width="620">
      <br><strong>Playwright surface beside WebDriver</strong>
      <br>Browser actions, element actions, assertions, tracing, replay, screenshots, and Doctor hooks share the SHAFT workflow.
    </td>
    <td width="50%">
      <img src="shaft-engine/src/main/resources/modular-era-feature-catalog/doctor-heal-trace.png" alt="Doctor Heal and Trace tools" width="620">
      <br><strong>Doctor, Heal, and Trace</strong>
      <br>Failure analysis, trace summaries, locator recovery, and reviewed repair suggestions stay connected to test evidence.
    </td>
  </tr>
  <tr>
    <td width="50%">
      <img src="shaft-engine/src/main/resources/modular-era-feature-catalog/api-reporting.png" alt="API contract reporting and observability APIs" width="620">
      <br><strong>API, contracts, and reporting</strong>
      <br>GraphQL, retries, typed JSON mapping, contract replay, locator health, trace reporting, and evidence profiles are grouped together.
    </td>
    <td width="50%">
      <img src="shaft-engine/src/main/resources/modular-era-feature-catalog/module-map.png" alt="SHAFT module map from current reactor POMs" width="620">
      <br><strong>Module map</strong>
      <br>Current reactor POMs show the lean core, BOM, relocation artifact, and optional module split.
    </td>
  </tr>
</table>

## What Changed

| Area | Framework-user value | Proof |
| --- | --- | --- |
| Lean modular core | Adopt the core engine first, then add BrowserStack, visual, video, SikuliX desktop image automation, AI, Doctor, Heal, Capture, or MCP only when a project needs them. | `module-map.png` |
| MCP automation surface | Drive WebDriver, Playwright, mobile, recording, coding-partner planning, target discovery, guide search, generated-code review, evidence manifests, and failure triage through one local server. | `mcp-tools.png` |
| IntelliJ IDEA plugin | First-run MCP setup now opens as a simple vertical click-through: pick the agent, copy the installer command, run it in the IntelliJ terminal, and check setup while the plugin uses installer defaults to find and persist the local SHAFT MCP launch command automatically. Guided now includes a Partner section that plans repository-aware work from intent, current source, selected code, and evidence before codegen starts; the Assistant restores safe project chats, uses a compact `Codex CLI` agent label with the full agent tooltip, keeps action/timeline chrome hidden until useful, uses a taller prompt composer with a concise placeholder, keeps Copy, Clear, and Rerun available as compact transcript icons when useful, renders code blocks with theme-aware light/dark palettes, keeps status text accessible, supports Ctrl+Enter / Command+Enter send plus Escape cancel, and keeps reviewed recorder/codegen actions, Inspector checks, Triage, Evidence Tools, project actions, settings/provider controls, and live-refresh Advanced Tools behind the same verified MCP setup gate. | `intellij-plugin-mcp-setup.png`, `intellij-plugin-mcp-setup-success.png`, `intellij-plugin-mcp-setup-error-dark.png`, `intellij-plugin-mcp-setup-narrow-dark.png`, `intellij-plugin-assistant.png`, `intellij-plugin-assistant-empty.png`, `intellij-plugin-assistant-dark.png`, `intellij-plugin-guided.png`, `intellij-plugin-triage.png`, `intellij-plugin-advanced-tools.png`, `intellij-plugin-settings.png`, `intellij-plugin-settings-dark.png` |
| IntelliJ Assistant fix for issue #3188 | Chat transcript messages are now role-rendered (user/assistant bubbles), sessions persist and restore correctly, and the route/runtime controls lock with a visible configure recovery path once MCP is configured. | `intellij-plugin-assistant.png`, `intellij-plugin-mcp-setup.png` |
| IntelliJ Assistant fix for issue #3237 | Local Agent completion callbacks now clear the running state even when the streaming token was already reset, and approved capture generation tells the Agent to preserve the recorded journey, open the first search result, and assert the final page title or page-specific text. | `intellij-plugin-assistant-live-output-dark.png`, `intellij-plugin-assistant-narrow-dark.png` |
| IntelliJ Assistant chat polish | Newly sent user prompts scroll into view immediately, while SHAFT-owned icon buttons and Assistant bubbles use borderless surfaces so chat feedback reads clearly in light, dark, and narrow tool windows. | `intellij-plugin-assistant.png`, `intellij-plugin-assistant-dark.png`, `intellij-plugin-assistant-narrow-dark.png` |
| IntelliJ and MCP hardening for issue #3244 | Onboarding preflight now validates Windows JDK21 `JAVA_HOME` state and auto-creates `%JAVA_HOME%\\Packages` when possible, then reports clear missing/invalid/unwritable diagnostics; recorder re-record flows map discard to `capture_stop` with `discard=true`; and `shaft_project_create` supports optional `shaftVersion` with blank/null defaulting to the latest published stable `shaft-engine` while bundled example POMs remain reactor-versioned. | `intellij-plugin-mcp-setup.png`, `intellij-plugin-mcp-setup-error-dark.png`, `intellij-plugin-recorder.png`, `mcp-tools.png` |
| Non-SHAFT-project hardening for issues #3396/#3397 | Every SHAFT-only Assistant action now gates on a real SHAFT dependency: tool approvals ("approve once/tool/all") persist per project instead of application-wide, the MCP connection heartbeat no longer polls in non-SHAFT projects, and mutating Assistant tools (`shaft_project_upgrade`, `verify_run_focused`, healer/doctor triage) return an onboarding nudge toward Create/Upgrade SHAFT Project instead of running. Settings > SHAFT "Test Connection" now probes the open project's workspace instead of the IDE process's own working directory. `healer_run_failed_test` fails fast with a guardrail message instead of burning guarded Maven builds when the target repository has no SHAFT dependency, and SHAFT MCP workspace errors for a missing Allure/report path now read as "no SHAFT setup found here" instead of a sandbox-escape message. | `intellij-plugin-assistant.png`, `intellij-plugin-assistant-approval-prompt.png`, `intellij-plugin-settings.png`, `intellij-plugin-settings-dark.png` |
| IntelliJ trust/polish for issue #3601 (remaining scope) | Setup wizard: finished steps collapse to a compact line once a later step is active (click or Enter/Space to re-expand), the Target/Runtime line reads as a de-emphasized caption with "MCP" spelled out on first mention, the six steps are threaded by a connecting rule, raw probe output on failure hides behind a "Show details" toggle, and a non-destructive "Connection & agents" button re-checks readiness without the "Reset everything" factory reset. Settings: the MCP stdio command field is read-only once a wizard-set command exists, with an "Edit manually" gate before direct editing. Assistant: the first-run welcome no longer uses list-item emoji and now orients first-time users to the icon-only toolbar, "Allow source edits" gets a distinct bordered treatment from the neutral Verbose/Auto-compact toggles, a dismissible hint points fresh (non-SHAFT) projects at the "Create a new SHAFT project" template, and tool results now show narrative Markdown with a per-message "Show raw output" disclosure toggle for the underlying raw evidence (A5) instead of only a bulk transcript-export appendix. Guided: a dismissible soft prompt nudges collapsed-view users toward Advanced options for templates/backend controls. Accessibility: informational labels across the setup wizard, Assistant, Settings, and Guided panels now expose their live, dynamically-updated text to screen readers via an accessible description that tracks every `setText` update, not just a static category name (issue #3603; remaining same-shape labels tracked separately as #3605). | `intellij-plugin-mcp-setup.png`, `intellij-plugin-mcp-setup-success.png`, `intellij-plugin-mcp-setup-error-dark.png`, `intellij-plugin-mcp-setup-narrow-dark.png`, `intellij-plugin-assistant.png`, `intellij-plugin-assistant-empty.png`, `intellij-plugin-assistant-dark.png`, `intellij-plugin-assistant-tool-result-raw-output.png`, `intellij-plugin-guided.png`, `intellij-plugin-settings.png`, `intellij-plugin-settings-dark.png` |
| IntelliJ Phase 4/5 hardening for issues #3634-#3642 | Success status color now passes WCAG contrast in dark themes; the Pin Response Fields dialog is a proper modal `DialogWrapper`; the selected workflow view and feature-panel split position persist across restarts; day-to-day action buttons carry per-action notification titles instead of a generic "SHAFT" banner; the API recording session panel (the last panel with visible-text buttons, now that every other one is icon-only) gets Alt-reachable mnemonics for Stop/Pin Fields/Generate/Copy CA Certificate; Guided's recorded-step list gains Delete/Move Up/Move Down for Playwright and Mobile recordings (shown below); the SHAFT Tests tab gains double-click Run and Ctrl+double-click Navigate on any row; watch mode now correlates a saved file against the last run's target class before replaying, offering a "Run `<Class>`" action on a mismatch instead of silently rerunning a stale test; and Doctor/Healer evidence screenshots render inline in the assistant transcript instead of requiring a separate file open. | `intellij-plugin-guided.png` |
| IntelliJ per-run execution overrides for issue #3659 | Adds a `RunConfigurationExtension`-backed "SHAFT" tab to IntelliJ's own TestNG/JUnit run-configuration editor, so a single run configuration can override `targetBrowserName`/`headlessExecution`/extra `-D` args without editing `custom.properties`; state round-trips through the run configuration's own saved XML and survives "Copy Configuration". First step of the IntelliJ plugin UX program tracked by issue #3666. | `intellij-plugin-run-config-shaft-tab.png` |
| SikuliX desktop automation | Desktop image automation lives in `shaft-sikulix`, so projects opt into `com.sikulix:sikulixapi` only when they need image-based desktop flows. | `tools/modularization/consumer-fixtures/sikulix/pom.xml`, `tests/scripts/test_sikulix_module_boundary.py` |
| Recorder-to-code workflow | Capture real user actions, preserve edited step intent and checkpoints, plan reuse with `shaft_coding_partner_plan`, then generate TestNG replay snippets, setup/assertion/control-flow review blocks, locator/action summaries, patch previews, backend comparisons, evidence manifests, and Page Object insertions. | `web-recorder.png`, `capture-catalog.png`, `intellij-plugin-guided.png` |
| Locator-first mobile recording | Resolve Appium Inspector pointer gestures through the accessibility tree, then generate ranked locator inventories, Page Object drafts, and record-at-target snippets before using coordinate fallback. | `android-recorder-working.png`, `android-recorder-locator-details.png` |
| Evidence-led failure work | Combine Allure failure briefs, traces, locator health, healing reports, and optional reviewed AI advice. | `doctor-heal-trace.png`, `api-reporting.png` |

## IntelliJ Coding Partner Top 10 Implementation

The recorder/codegen handoff is now a three-iteration coding-partner loop before
any source edit: plan the working set and user steps, reuse the recommended Java
owner and insertion anchor, avoid duplicate locators/actions/classes, prove
missing browser steps, inspect the patch preview, collect evidence, then verify
locally. Generated code uses Smart Locators and the SHAFT locator builder before
native `By.xpath(...)`; it must not emit `SHAFT.GUI.Locator.xpath(...)`.

The public entry point is IntelliJ, not raw MCP: Assistant `/partner` and Guided
`Plan coding partner` gather the IDE context, then MCP returns the reuse plan,
reviewed code blocks, and focused verification command.

```mermaid
flowchart LR
    IDE[IntelliJ Assistant / Guided] --> Plan[shaft_coding_partner_plan]
    Plan --> Reuse[Existing target + insertion anchor]
    Reuse --> Review[Reviewed code blocks / patch preview]
    Review --> Verify[Focused local verification]
```

| Rank | Enhancement | Why users feel it immediately |
| --- | --- | --- |
| 1 | Coding Partner Workspace | Guided exposes `Plan coding partner` and Assistant exposes `/partner`, so intent, backend, current Java source, selected text, and evidence paths become one preview-only MCP request. |
| 2 | Repository Reuse Scanner | `shaft_coding_partner_plan` and `capture_target_candidates` return existing tests/page objects with driver variables, anchors, locator summaries, and action summaries. |
| 3 | Duplicate Prevention Plan | The plan separates `reuseMatches` from `missingCodeItems` and returns `recommendedTargetSourcePath` plus `recommendedInsertionAnchor`, so agents extend existing owners before creating new page objects or generated tests. |
| 4 | Context Plan Generator | `workingSetSummary` records the active intent, backend, source path, candidate count, and evidence count; `stepPlan` breaks the user request into proofable browser/code steps. |
| 5 | Live Browser Step Runner | Suggested calls route WebDriver work through `browser_open_intent` and Playwright work through `playwright_browser_get_page_dom` before publishing locators. |
| 6 | Missing-Code Generator | `missingCodeItems` calls out raw Selenium conversion, missing stable locators, missing Java targets, or absent user intent before generation. |
| 7 | Patch Preview And Apply | `capture_record_at_target_code_blocks` and `mobile_record_at_target_code_blocks` still return preview-only diff blocks with apply order; existing locator fields and duplicate action lines are reused or skipped before IntelliJ approval. |
| 8 | Focused Verification | The plan returns the smallest useful verification command, then Capture/Doctor evidence links compile or replay failures back to source and recording context. |
| 9 | Failure Repair Loop | Failure skills now route Doctor, trace, and healer evidence through the coding-partner plan before changing shared page/test code. |
| 10 | Evidence Pack And PR UX | `capture_evidence_pack` plus refreshed IntelliJ screenshots provide local source/report/review/screenshot evidence without zipping or uploading artifacts. |

## IntelliJ + MCP UX Enhancements (Next Wave)

Building on the coding-partner loop, this wave tightens the last-mile experience:
plan to applied code to verified result, with a simpler default command surface
and sharper trust signals.

| Enhancement | What users get | Entry point |
| --- | --- | --- |
| Minimal command surface | The Assistant shows only five core commands by default — `/record-web`, `/record-mobile`, `/codegen`, `/doctor`, `/upgrade`. Everything else (including the new `/verify` and `/skills`) moves behind Expert mode, which is the existing advanced-UI switch. Natural-language intents still work in both modes. | Assistant composer; `Settings -> SHAFT` Expert mode |
| Preview-only patch diff | `shaft_coding_partner_diff` turns reviewed SHAFT code blocks into a unified diff against an existing Java target and anchor, without writing files. | `shaft_coding_partner_diff` |
| Focused verification runner | `verify_run_focused` runs the plan's smallest Maven verification command headlessly through the Healer allowlist and returns a bounded pass/fail summary; the Expert `/verify` command routes to it. | `verify_run_focused`, `/verify` |
| Provider readiness | `autobot_provider_status` reports the configured provider, model, API-key presence (never the value), and structured-output support for an at-a-glance readiness view. | `autobot_provider_status` |
| Structured cloud codegen | `autobot_provider_chat` now returns a structured JSON-schema codegen response — summary, per-block code with target path/anchor, cited SHAFT guide URLs, unverified locator assumptions, and a guardrail status computed by running SHAFT guardrails on the returned code. | `autobot_provider_chat` |
| Readable tool output | The Assistant renders the coding-partner plan, diff preview, verification result, provider status, and structured codegen as clean markdown with inline citations and locator-assumption warnings instead of raw JSON. | `Assistant` tab |
| Fix Failing Test | The Triage panel adds a one-click button that routes the Allure/trace evidence and locator source into `shaft_coding_partner_plan`, bridging analyze -> plan -> verify. | `Triage` tab |
| Optimized authoring skills | The `shaft-skills` set gains `verifying-and-applying-shaft-changes` (review -> diff -> apply -> guardrails -> verify) and the existing skills point their apply/verify step at the new tools; the Expert `/skills` command lists them in-panel. | `shaft-skills/`, `/skills` |

```mermaid
flowchart LR
    Intent[Intent in IntelliJ] --> Plan[shaft_coding_partner_plan]
    Plan --> Diff[shaft_coding_partner_diff preview]
    Diff --> Apply[Approve + apply in IDE]
    Apply --> Guard[test_code_guardrails_check]
    Guard --> Verify[verify_run_focused]
    Verify -->|fail| Triage[Fix Failing Test]
    Triage --> Plan
```

### Settling Pass: Audit + UI/UX Polish

A follow-up pass re-audited this wave's own gap-analysis fixes (all confirmed intact) and closed the remaining
professionalism gaps the first audit didn't reach — no new features, all reused-vocabulary polish.

| Enhancement | What users get | Entry point |
| --- | --- | --- |
| Unified status presentation | One shared `ShaftStatusPresentation` class now owns every status color and glyph (success/pending/progress/error, plus the warning glyph) that three panels used to reimplement independently. | Settings, MCP setup, Assistant |
| Colored, iconed timeline | The Assistant's run timeline shows a checkmark/hourglass/cross next to Completed/Running/Failed steps instead of plain text, reusing the same shared vocabulary. | `Assistant` run timeline |
| Consistent section headers | Guided Workflows and Evidence Triage's intro captions, and the Workflow selector's field label, now get the same bold-header treatment Settings already had. | `Guided`, `Triage` tabs |
| Field tooltips | Evidence Triage's six input fields (Allure, Trace, Repository, Sources, Locator source, Command) now show a tooltip, matching every other panel in the plugin. | `Triage` tab |
| Iconed workflow selector | The Workflow selector — the plugin's primary navigation control across all 8 destinations — now shows an icon per destination instead of bare text. | `Workflow` selector |
| Hardened narrow layout | The Assistant status row now trims safely at the 360px narrow tool-window width instead of risking a clip, keeping the full text in a tooltip. | `Assistant` tab, narrow layout |
| Muted empty state | The tool catalog's "No tool template matches the current filter" message now renders muted/disabled, matching the plugin's established empty-state treatment. | `Advanced` tools tab |
| Unified transient confirmation | The Assistant's transient status label now uses the same bordered, tinted "chip" look as the MCP setup toast, instead of two different visual languages for the same concept. | `Assistant` tab, `Settings \| SHAFT MCP` setup |
| Explicit icon wiring | Guided Workflows' and Evidence Triage's buttons now receive their icon explicitly at construction instead of an icon lookup keyed on the button's display text, so a future label edit can't silently point at the wrong icon. | `Guided`, `Triage` tabs |

## Modular Adoption

Use the new reactor split when you want SHAFT as a framework base, not a monolith. `shaft-engine` remains the center. Optional modules publish independently, the BOM keeps dependency alignment boring, and `legacy-shaft-engine` preserves the relocation path for existing consumers.

| Capability | What changed | Try it |
| --- | --- | --- |
| Lean core | `shaft-engine` contains the core WebDriver/Appium, API, and reporting surfaces without forcing every optional integration into the same artifact. | `mvn -pl shaft-engine -am package '-DskipTests'` |
| Optional modules | BrowserStack, video, visual, SikuliX, AI, Doctor, Heal, and Capture are opt-in modules. | `mvn -pl shaft-browserstack,shaft-video,shaft-visual,shaft-sikulix -am package '-DskipTests'` |
| Desktop image automation | `shaft-sikulix` owns the `com.sikulix:sikulixapi` dependency and keeps image-based desktop automation out of the lean engine artifact. | `mvn -f tools/modularization/consumer-fixtures/sikulix/pom.xml test '-DskipTests'` |
| Upgrade path | `shaft-bom` and `legacy-shaft-engine` keep dependency alignment and relocation explicit. | `rg "shaft-bom|relocation" shaft-bom legacy-shaft-engine -g pom.xml` |
| Consumer fixture | A combined-module fixture validates that optional modules can be consumed together. | `mvn -f tools/modularization/consumer-fixtures/combined-modules/pom.xml test '-DskipTests'` |

<img src="shaft-engine/src/main/resources/modular-era-feature-catalog/module-map.png" alt="Current module map" width="760">

## MCP and Agent Workflows

`shaft-mcp` turns SHAFT into an agent-friendly local automation server. Framework users get direct tools for browser sessions, Playwright sessions, mobile devices, recorder sessions, coding-partner planning, guide search, scenario discovery, generated-code guardrails, trace reading, Doctor analysis, and Heal workflows.

| Workflow | What it gives users | Entry point |
| --- | --- | --- |
| Install and run | Local installers for Codex, Claude, Claude Desktop, Copilot, Copilot IntelliJ, plus installer defaults that the IntelliJ plugin can use to find the generated stdio argfile automatically. The Marketplace plugin itself does not run installer scripts. | `py -3 scripts/mcp/install_shaft_mcp.py --client intellij-plugin --json` |
| URL intent orientation | Open a URL, bound the DOM, rank actionable elements, return SHAFT locator code, and suggest the next MCP tools. | `driver_initialize -> browser_open_intent(targetUrl, userIntent, 200000, 10)` |
| Coding partner plan | Summarize the current IntelliJ/user intent, rank existing Java targets, return a structured `stepPlan`, recommend a target source/anchor, list missing code, suggest MCP proof calls, and return a focused verification command. | `shaft_coding_partner_plan(repositoryPath=".", intent="login", currentSourcePath="src/test/java/...")` |
| Locator inspection | Reuse `shaft-capture` `LocatorRanker` scoring for role, accessible name, label, test id, id, name, CSS, and XPath alternatives. | `bestLocator.strategy=ROLE; shaftLocatorCode=SHAFT.GUI.Locator.clickableField("Sign in")` |
| Capture review blocks | Return setup prerequisites, assertion suggestions, locator alternatives, action sequences, locator-confidence queues, validation back-links, and control-flow review notes as additive MCP code blocks after generation. | `capture_code_blocks`, `capture_record_at_target_code_blocks` |
| Semantic actions | Combine guide search, scenario catalog, guardrail checks, and `natural_act` without leaving the MCP session. | `shaft_guide_search`, `test_automation_scenarios`, `test_code_guardrails_check`, `natural_act` |
| Playwright MCP and CLI parity | Official Playwright MCP/CLI can be used as a delegated exploration sidecar for accessibility snapshots, browser commands, network/storage/devtools, codegen, and Test Agent planning; SHAFT converts the proven behavior into Java Page Objects, Capture sessions, Doctor/Heal evidence, and `SHAFT.GUI.Playwright` or WebDriver code. | `test_automation_scenarios(area="playwright")`, `capture_codegen_features`, `capture generate --backend playwright` |

```text
driver_initialize(browserName="chrome", headless=true)
browser_open_intent(
  targetUrl="https://example.com/login",
  userIntent="click sign in",
  maxCharacters=200000,
  maxElements=10
)

orientation.elements[0].bestLocator.strategy=ROLE
orientation.elements[0].shaftLocatorCode=SHAFT.GUI.Locator.clickableField("Sign in")
nextTools=[browser_get_page_dom, browser_take_screenshot, shaft_guide_search, element_click, natural_act, capture_start, capture_code_blocks, test_code_guardrails_check]
```

```mermaid
flowchart LR
    Intent[User intent in IntelliJ] --> Route{Need SHAFT Java edit?}
    Route -->|Yes| Plan[shaft_coding_partner_plan]
    Route -->|Browser exploration sidecar| PW[Official Playwright CLI or MCP]
    PW --> Evidence[Snapshots, locators, screenshots, network, storage, traces]
    Evidence --> Plan
    Plan --> Capture[Capture or Playwright code blocks]
    Capture --> POM[Existing Page Objects and tests]
    POM --> Guardrails[test_code_guardrails_check]
    Guardrails --> Verify[Focused Maven or Gradle validation]
```

<img src="shaft-engine/src/main/resources/modular-era-feature-catalog/mcp-tools.png" alt="MCP tool manifest" width="760">

## IntelliJ IDEA Plugin

`shaft-intellij` is the stable IntelliJ IDEA plugin (`io.github.shafthq.shaft`, `10.3.20260716`). It is the public front door for coding-partner work: start in Assistant or Guided, let MCP plan reuse, review generated code blocks, and run the focused verification command. It is intentionally thin: first-run setup defaults to Codex CLI, walks through `Pick agent`, `Copy command`, `Run in terminal`, and `Check setup`, then uses installer defaults to find and persist the local SHAFT MCP launch command automatically before revealing `Start chatting`. The plugin does not download or execute installer scripts at runtime. Settings remain available later for Local/Cloud routing, API keys, and custom local MCP commands.

| Surface | What users get | Entry point |
| --- | --- | --- |
| Tool window | Right-side SHAFT assistant panel with dedicated workflow tabs for Assistant, Guided, Recorder, Inspector, Triage, Evidence Tools, Projects, and Advanced Tools. | `Tools -> SHAFT -> Open SHAFT` |
| First-run setup | If a tested MCP command has passed setup, the tool window opens the Assistant directly. Otherwise, setup opens with `Connect SHAFT Assistant`, defaults to Codex CLI, and shows a simple stepper: `1 Pick agent`, `2 Copy command`, `3 Run in terminal`, `4 Check setup`, then `Ready`. Users never see or paste the managed stdio command; `Check setup` discovers it from the installer output paths, verifies the selected local agent, and reveals `Start chatting` on success. Retry actions stay enabled after failures, with categorized diagnostics that avoid exposing the managed command. | `Tools -> SHAFT -> Open SHAFT` |
| Settings and providers | Retest MCP, change Local/Cloud routing, select the cloud provider/model, and store or clear OpenAI, Anthropic, Gemini, and GitHub keys. Only the selected cloud provider key is passed to MCP. | `Settings -> SHAFT` |
| Guided workflows | Pick starter templates for browser capture to Page Objects, failed Allure analysis, Selenium-to-SHAFT conversion, confirmed new SHAFT projects, and locator inspection without editing JSON first; the recorder action now says `Review code` because it returns reviewed SHAFT code blocks. | `Guided` tab |
| Recorder | Editable JSON templates for `capture_start`, target discovery, record-at-target patch preview, backend comparison, evidence packs, checkpoints, replay generation, Playwright replay, and mobile replay. | `Recorder` tab |
| Project tools | Create a SHAFT project after confirmation, preview an upgrade, or apply an approved upgrade through MCP. | `Projects` tab |
| Triage | Prepare failed Allure analysis, trace analysis, Doctor fix suggestions, Healer runs, and locator proposal requests. | `Triage` tab |
| Evidence and healing | Run failed-test analysis, trace lookup, trace analysis, and healer templates from the IDE. | `Evidence Tools` tab |
| Mobile Inspector | Check mobile tooling, prepare Inspector recording, inspect status, read accessibility trees, and take mobile screenshots. | `Inspector` tab |
| Agent helpers | Switch the Assistant composer between Ask, Plan, and guarded Agent prompts through local Codex, Claude Code, or Copilot CLI; Ask/Plan can route to OpenAI, Anthropic, Gemini, or GitHub Models with a stored key; Cloud Agent is demoted to Plan because provider chat cannot mutate the local workspace; project chats persist rendered messages without raw payloads; code writing and conversion prompts are scoped to the current editor file; the compact `Codex CLI` label exposes `Agent: Local / Codex / CLI` as a tooltip; the command picker shows summaries/examples and the context button exposes `@` and `#` workflow/project context without filling empty chats with starter text; action and timeline chrome stays hidden until useful; Assistant status text is exposed to assistive technologies. | `Assistant` tab |
| Record at target | Open the side panel, prefill `capture_record_at_target_code_blocks` from the Java caret context, then review code blocks, inspect the patch preview, apply intentionally, and verify. | `Recorder` tab |
| Editor action | Start from the current Java caret context through the `Record SHAFT Flow Here` action in Tools or the editor popup menu; it now uses `recordings/intellij-capture.json` instead of a placeholder session path. | `Record SHAFT Flow Here` |
| Run-configuration overrides | A "SHAFT" tab on IntelliJ's native TestNG/JUnit run-configuration editor: opt in per run, then override `targetBrowserName`, `headlessExecution`, and extra `-D` VM args for that one run without touching `custom.properties`. Untouched by default, so every existing run configuration keeps inheriting the project file. | `Run/Debug Configurations -> SHAFT` |

The plugin rides as a right-side IntelliJ panel, similar to assistant tools such as GitHub Copilot. The Assistant keeps Ask/Plan/Agent mode switching in the bottom composer, gates source mutation behind the local Agent approval checkbox, restores project chat sessions without persisting raw MCP payloads, scrolls newly sent prompts into view before long-running responses finish, uses compact JetBrains-style icons for dense controls including Copy all, Clear, and Rerun, turns the submit control into an animated spinner with hover-to-cancel while running, changes Cancel into Kill after the first cancellation request so a second click terminates the active process immediately, clears the running state when a local Agent result arrives, keeps code blocks visually distinct from chat bubbles with a light editor-style palette in light mode and a dark surface in dark mode, anchors Send at the bottom-right with Ctrl+Enter, Command+Enter, and Ctrl+click shortcuts, lets Escape cancel a running request from the Assistant view, keeps empty chats focused on the composer, renders command summaries/examples in the picker, exposes `@workflow` and `#project` context through a dedicated context button, and exposes the selected local agent as compact text such as `Codex CLI` with `Agent: Local / Codex / CLI` in the tooltip. The run timeline and action chrome stay hidden until a prompt, tool run, approval, completion, cancellation, or failure gives them useful state. The Advanced Tools and Recorder tabs still show exact JSON arguments, validate them before run, require the same verified MCP setup state as the Assistant, and write formatted outputs to an output pane.

Assistant command examples:

```text
/codegen recordings/intellij-capture.json
/record-web https://example.com
review recording recordings/intellij-capture.json
/record-mobile inspector Android recordings/inspector.json
/doctor target/allure-results
/guide locators
/guardrails driver.element().click(locator);
/browser open https://example.com sign in
/project upgrade .
```

<table>
  <tr>
    <td width="50%"><img src="shaft-engine/src/main/resources/modular-era-feature-catalog/intellij-plugin-assistant.png" alt="IntelliJ plugin Assistant tab" width="620"></td>
    <td width="50%"><img src="shaft-engine/src/main/resources/modular-era-feature-catalog/intellij-plugin-assistant-empty.png" alt="IntelliJ plugin Assistant empty composer" width="620"></td>
  </tr>
  <tr>
    <td width="50%"><img src="shaft-engine/src/main/resources/modular-era-feature-catalog/intellij-plugin-assistant-dark.png" alt="IntelliJ plugin Assistant tab in dark theme" width="620"></td>
    <td width="50%"><img src="shaft-engine/src/main/resources/modular-era-feature-catalog/intellij-plugin-assistant-live-output-dark.png" alt="IntelliJ plugin Assistant tab while a local agent is running" width="620"></td>
  </tr>
  <tr>
    <td width="50%"><img src="shaft-engine/src/main/resources/modular-era-feature-catalog/intellij-plugin-assistant-narrow-dark.png" alt="IntelliJ plugin Assistant tab in a narrow dark tool window" width="620"></td>
    <td width="50%"><img src="shaft-engine/src/main/resources/modular-era-feature-catalog/intellij-plugin-guided.png" alt="IntelliJ plugin Guided tab" width="620"></td>
  </tr>
  <tr>
    <td width="50%"><img src="shaft-engine/src/main/resources/modular-era-feature-catalog/intellij-plugin-triage.png" alt="IntelliJ plugin Triage tab" width="620"></td>
    <td width="50%"><img src="shaft-engine/src/main/resources/modular-era-feature-catalog/intellij-plugin-recorder.png" alt="IntelliJ plugin Recorder tab" width="620"></td>
  </tr>
  <tr>
    <td width="50%"><img src="shaft-engine/src/main/resources/modular-era-feature-catalog/intellij-plugin-inspector.png" alt="IntelliJ plugin Inspector tab" width="620"></td>
    <td width="50%"><img src="shaft-engine/src/main/resources/modular-era-feature-catalog/intellij-plugin-evidence.png" alt="IntelliJ plugin Evidence Tools tab" width="620"></td>
  </tr>
  <tr>
    <td width="50%"><img src="shaft-engine/src/main/resources/modular-era-feature-catalog/intellij-plugin-projects.png" alt="IntelliJ plugin Projects tab" width="620"></td>
    <td width="50%"><img src="shaft-engine/src/main/resources/modular-era-feature-catalog/intellij-plugin-advanced-tools.png" alt="IntelliJ plugin Advanced Tools tab" width="620"></td>
  </tr>
  <tr>
    <td width="50%"><img src="shaft-engine/src/main/resources/modular-era-feature-catalog/intellij-plugin-advanced-tools-dark.png" alt="IntelliJ plugin Advanced Tools tab in dark theme" width="620"></td>
    <td width="50%"><img src="shaft-engine/src/main/resources/modular-era-feature-catalog/intellij-plugin-mcp-setup.png" alt="IntelliJ plugin first-run MCP setup flow" width="620"></td>
  </tr>
  <tr>
    <td width="50%"><img src="shaft-engine/src/main/resources/modular-era-feature-catalog/intellij-plugin-mcp-setup-success.png" alt="IntelliJ plugin MCP setup success flow" width="620"></td>
    <td width="50%"><img src="shaft-engine/src/main/resources/modular-era-feature-catalog/intellij-plugin-mcp-setup-error-dark.png" alt="IntelliJ plugin MCP setup error state in dark theme" width="620"></td>
  </tr>
  <tr>
    <td width="50%"><img src="shaft-engine/src/main/resources/modular-era-feature-catalog/intellij-plugin-mcp-setup-narrow-dark.png" alt="IntelliJ plugin MCP setup flow in a narrow dark tool window" width="620"></td>
    <td width="50%"><img src="shaft-engine/src/main/resources/modular-era-feature-catalog/intellij-plugin-settings.png" alt="IntelliJ plugin Settings panel" width="620"></td>
  </tr>
  <tr>
    <td width="50%"><img src="shaft-engine/src/main/resources/modular-era-feature-catalog/intellij-plugin-settings-dark.png" alt="IntelliJ plugin Settings panel in dark theme" width="620"></td>
    <td width="50%"><img src="shaft-engine/src/main/resources/modular-era-feature-catalog/intellij-plugin-mcp-guide.png" alt="IntelliJ plugin Guide MCP category" width="620"></td>
  </tr>
  <tr>
    <td colspan="2">Screenshots show the 860 px right-side panel, 360 px narrow Assistant and setup views, first-run MCP installer/default/success/error states, the empty Assistant composer, live assistant output with run timeline, Guided starter templates, command hint affordance, and SHAFT settings panel captured on the current plugin branch in standard IntelliJ light and dark themes.</td>
  </tr>
</table>

## Capture and Code Generation

Capture is now the bridge between exploratory testing and maintainable Java. It records actions, checkpoints, browser context, privacy choices, network details, locator alternatives, replay metadata, and code-generation warnings in a deterministic session model.

| Capability | Better user outcome | Entry point |
| --- | --- | --- |
| Managed web recorder | Pause, assert, verify, edit, delete, reorder, add visible assertions from captured targets, pick locators, and see readiness score while recording. | `capture_start --url https://example.com --browser chrome --output target/capture/session.json --session-goal "record checkout"` |
| TestNG replay | Generate replay snippets, intent-derived class/method names, source review headers, Page Object insertions, and review warnings from the captured session. | `capture_generate_replay --session target/capture/session.json --target-source src/test/java/CheckoutTest.java` |
| Assertions | Record browser and element verification events from in-panel dialogs; checkpoint notes do not replace generated SHAFT assertion-builder calls. | `capture_checkpoint --description "cart total is visible"` |
| Locator refinement | Keep fallback locator replay, live locator picker/refinement, compact generated fallback helpers, and ranked locator alternatives in the generated review blocks. | `capture_code_blocks --session target/capture/session.json --driver-variable-name driver` |
| Review workbench | Review blockers, assertions, locator decisions, Page Object draft, copyable commands, code-block summary, and control-flow suggestions before reading or saving source. | `target/shaft-capture/capture-workbench.html` |
| Record at cursor | Generate code blocks for automated and user-performed flows, including record-at-target snippets. | `capture_record_at_target_code_blocks` |
| Browser context | Capture viewport, device, color scheme, geolocation, language, timezone, storage, HTTPS, HAR, proxy, and user data directory options. | `capture start --url https://example.com --device "Pixel 5" --timezone Africa/Cairo --save-har target/capture/run.har` |

```text
Load current shaft-capture-recorder.js into a local fixture page.
Type email, select plan, type notes, toggle terms, submit.
Capture overlay state: RISKY | 8 events | Step 8 needs a follow-up assertion after form submission.
Step inspector: edit, delete, move up/down, or add a visible assertion from the captured target.
Generated replay syntax: driver.element().click(SHAFT.GUI.Locator.inputField("Username"));
```

<table>
  <tr>
    <td width="50%"><img src="shaft-engine/src/main/resources/modular-era-feature-catalog/web-recorder.png" alt="Web recorder evidence" width="620"></td>
    <td width="50%"><img src="shaft-engine/src/main/resources/modular-era-feature-catalog/capture-catalog.png" alt="Capture catalog evidence" width="620"></td>
  </tr>
</table>

## Mobile Automation

Mobile support now covers Appium sessions, mobile web emulation, accessibility trees, screenshots, contexts, rotation, keyboard control, app backgrounding, toolchain diagnostics, and Inspector-based recording. The key improvement is locator-first recording: pointer gestures are resolved through the Appium accessibility source before coordinate fallback is used.

| Capability | Better user outcome | Entry point |
| --- | --- | --- |
| Native Appium sessions | Initialize native sessions, inspect accessibility, and tap by stable locators. | `mobile_initialize_native -> mobile_get_accessibility_tree -> mobile_tap(ACCESSIBILITY_ID, "login")` |
| Toolchain diagnostics | Check Appium, Inspector plugin, adb, emulator, sdkmanager, avdmanager, and iOS/macOS readiness from one tool. | `mobile_toolchain_status` |
| Inspector recording | Prepare, start, inspect status, control, stop, and generate code from wrapped Appium Inspector recording. | `mobile_inspector_record_prepare -> mobile_inspector_record_start -> mobile_inspector_record_stop` |
| Locator-first replay | Convert a tap into `ACCESSIBILITY_ID` and a swipe target into `ID` when the accessibility tree supports it. | `mobile_recording_code_blocks(recordingPath="target/shaft-evidence/mobile-inspector-locators.json")` |
| Mobile Page Object handoff | Return replay, ranked locator inventory, action sequence, Page Object draft, and focused insertion snippets for existing mobile POM classes. | `mobile_recording_code_blocks -> mobile_record_at_target_code_blocks` |

```text
mobile_initialize_native(appiumServerUrl="http://127.0.0.1:4723", platformName="Android", deviceName="emulator-5554")
mobile_inspector_record_start(...)
GET /session/{id}/source -> current Appium accessibility XML
POST /session/{id}/actions -> pointer tap inside content-desc="login"
POST /session/{id}/actions -> pointer swipe inside resource-id="com.example:id/list"
mobile_record_status() -> actionCount=2

Recorded action evidence:
tap -> locatorStrategy=ACCESSIBILITY_ID, locatorValue=login
swipeByOffset -> locatorStrategy=ID, locatorValue=com.example:id/list, xOffset=0, yOffset=200

SHAFT fluent replay excerpt:
driver.element().touch().tap(SHAFT.GUI.Locator.accessibilityId("login"));
driver.element().touch().swipeByOffset(SHAFT.GUI.Locator.id("com.example:id/list"), 0, 200);

Generated mobile handoff blocks:
mobile-replay-method
mobile-pom-locator-inventory
mobile-pom-action-sequence
mobile-page-object-draft
mobile-target-locator-fields
mobile-target-action-snippet
```

Coordinate fallback stays warning-only when no stable locator can be resolved:

```text
"Coordinate fallback used because no stable locator could be resolved from the accessibility tree; this will probably fail when executed on a different device, screen size, orientation, or app state."
```

<table>
  <tr>
    <td width="50%"><img src="shaft-engine/src/main/resources/modular-era-feature-catalog/android-emulator-device.png" alt="Android emulator evidence" width="620"></td>
    <td width="50%"><img src="shaft-engine/src/main/resources/modular-era-feature-catalog/android-toolchain-status.png" alt="Android toolchain evidence" width="620"></td>
  </tr>
  <tr>
    <td width="50%"><img src="shaft-engine/src/main/resources/modular-era-feature-catalog/android-recorder-working.png" alt="Android recorder evidence" width="620"></td>
    <td width="50%"><img src="shaft-engine/src/main/resources/modular-era-feature-catalog/android-recorder-locator-details.png" alt="Android locator details evidence" width="620"></td>
  </tr>
</table>

## Web, Playwright, API, and Contracts

The modular era keeps classic WebDriver/Appium flows, adds a first-class Playwright facade, and expands browser/API test composition. Users can mix browser network interception, API retry and mapping helpers, GraphQL, OpenAPI coverage, and contract recording without changing framework style.

| Surface | What changed | Example |
| --- | --- | --- |
| Playwright facade | `SHAFT.GUI.Playwright` sits beside WebDriver under the shared GUI driver concept. | `SHAFT.GUI.Playwright driver = new SHAFT.GUI.Playwright();` |
| Playwright parity | Browser actions, element actions, assertions, verifications, tracing, contract replay, natural action executor, screenshots, and Doctor hooks. | `playwright_capture_code_blocks`, `playwright_replay_recording`, `playwright_doctor_suggest_fix` |
| Capture CLI backend selection | The same persisted Capture session can now generate WebDriver or SHAFT Playwright replay from local CLI use. | `capture generate --session recordings/example.json --backend playwright` |
| Browser network control | Intercept, mock, assert, verify, throttle, block resources, bridge API/browser auth state, and record contracts. | `driver.browser().interceptRequest().get().urlContains("/api/users")...perform();` |
| API facade | GraphQL builder, retry policies, typed JSON mapping to classes/records/lists, and OpenAPI coverage thresholds. | `api.get("/health").withRetry(RetryPolicy.transientFailures().maxAttempts(3)).perform();` |
| Browser/mobile polish | UI state wait timeout, touch end-scroll, image invisibility waits, Appium recursion fallback, and mobile trace enrichment. | `SHAFT.Properties.timeouts.set().waitForUiStateTimeout(600);` |
| CLI and grid | SSH terminal sessions, SFTP, port forwarding, redaction, remote WebDriver timeout, Selenium Grid preflight, remote video, and BrowserStack app capability handling. | `SHAFT.CLI.remoteTerminal(host, 22, user, keyFolder, keyName, true);` |

```java
SHAFT.GUI.Playwright driver = new SHAFT.GUI.Playwright();
driver.browser().navigateToURL("https://example.com");
driver.element().click(By.id("submit"));
driver.assertThat().browser().title().contains("Example");
driver.quit();
```

```java
driver.browser()
      .interceptRequest()
      .get()
      .urlContains("/api/users")
      .respond()
      .statusCode(200)
      .jsonBody("{\"ok\":true}")
      .perform();
driver.browser().throttleNetwork(250, 64, 32);
driver.browser().blockNetworkResources("*.png", "*.jpg");
```

```java
SHAFT.Contracts.startRecording("src/test/resources/contracts/checkout.json", "/api/checkout");
api.post("/api/checkout").setRequestBody(order).perform();
SHAFT.Contracts.stopRecording();

SHAFT.Contracts.startAssertMode("src/test/resources/contracts/checkout.json");
api.post("/api/checkout").setRequestBody(order).perform();
SHAFT.Contracts.stopValidation();
```

```java
api.sendGraphQlRequest("/graphql", "query { viewer { id } }").perform();

api.get("/health")
   .withRetry(RetryPolicy.transientFailures().maxAttempts(3))
   .perform();

User user = api.get("/users/1").perform().getResponseAs(User.class);
```

<table>
  <tr>
    <td width="50%"><img src="shaft-engine/src/main/resources/modular-era-feature-catalog/playwright-surface.png" alt="Playwright surface evidence" width="620"></td>
    <td width="50%"><img src="shaft-engine/src/main/resources/modular-era-feature-catalog/api-reporting.png" alt="API and reporting evidence" width="620"></td>
  </tr>
</table>

## Doctor, Heal, Trace, and Reporting

Failure work is now a connected workflow instead of a pile of screenshots. Doctor analyzes failure evidence, Trace exposes structured browser and network context, Heal recovers locators with explainable reports, and reporting surfaces locator health, flake profiling, Allure 3 usability, evidence profiles, failure trace viewer, diagnostics bundles, and standardized HTML output.

| Tooling | What it helps with | Entry point |
| --- | --- | --- |
| Doctor | Deterministic failed-test analysis, evidence bundle parsing, root-cause categories, and a local CLI. | `doctor analyze --input allure-results --output-dir target/shaft-doctor` |
| MCP triage | Combine Allure briefs, SHAFT traces, stacktrace context, locator health, and flakiness categories before repairs. | `doctor_analyze_failed_allure -> trace_summarize -> doctor_analyze_trace -> healer_run_failed_test` |
| Reviewed repair advice | Optional AI-assisted advisory flow with repair workflow and draft PR guardrails. | `doctor propose-fix --analysis target/shaft-doctor/doctor-report.json --output-dir target/shaft-doctor` |
| Heal | Deterministic locator recovery for Selenium/Appium with explainable decisions, reports, and MCP healer tools. | `healer_run_failed_test`, `playwright_healer_run_failed_test`, `ShaftHeal.lastReport()` |
| Trace | Structured Selenium trace archive with actionability diagnostics, network, console, native metadata, and MCP trace tools. | `trace_latest -> trace_read -> trace_summarize -> doctor_analyze_trace` |
| Reporting | Failure trace viewer, diagnostics bundle, Allure failure briefs, full log attachments, locator health, flake profiler, evidence profiles, and lean assertion/action reporting. | `SHAFT.Properties.reporting.set().traceEnabled(true).traceMode("failure");` |

```java
SHAFT.Properties.reporting.set()
        .evidenceLevel("BALANCED")
        .locatorHealthEnabled(true)
        .locatorHealthWarnBelowScore(80)
        .traceEnabled(true)
        .traceMode("failure");
```

<table>
  <tr>
    <td width="50%"><img src="shaft-engine/src/main/resources/modular-era-feature-catalog/doctor-heal-trace.png" alt="Doctor Heal Trace evidence" width="620"></td>
    <td width="50%"><img src="shaft-engine/src/main/resources/modular-era-feature-catalog/api-reporting.png" alt="Reporting evidence" width="620"></td>
  </tr>
</table>

## Release and Developer Experience Guardrails

The modular-era work also tightened release and developer workflows: release guards, modular upgrader coverage, MCP runtime path fixes, generated-code guardrails, documentation consolidation, graph assets, and memory guidance now sit beside the feature surface instead of after it.

```powershell
py -3 scripts/ci/validate_agent_setup.py
py -3 scripts/ci/validate_shaft_mcp_transports.py
```

## Relationship Map

```mermaid
flowchart LR
    Core[shaft-engine<br/>lean core] --> WebDriver[Selenium/Appium WebDriver]
    Core --> Playwright[SHAFT.GUI.Playwright]
    Core --> API[API facade]
    Core --> Reporting[Allure 3, traces, reports]

    BOM[shaft-bom] --> Core
    Legacy[legacy-shaft-engine<br/>relocation] --> Core

    Capture[shaft-capture] --> Core
    Capture --> Recorder[web recorder UI]
    Capture --> Codegen[TestNG replay/code blocks]

    MCP[shaft-mcp] --> Core
    MCP --> Capture
    MCP --> Playwright
    MCP --> Mobile[Appium/mobile tools]
    MCP --> Doctor
    MCP --> Heal
    MCP --> Trace[trace tools]
    IntelliJ[shaft-intellij<br/>IDE plugin] --> MCP

    Doctor[shaft-doctor] --> Reporting
    Heal[shaft-heal] --> Core
    AI[shaft-ai + shaft-pilot-core] --> Doctor
    AI --> MCP

    BrowserStack[shaft-browserstack] --> Core
    Video[shaft-video] --> Core
    Visual[shaft-visual] --> Core
    SikuliX[shaft-sikulix<br/>desktop image automation] --> Core
```

<details>
<summary>Recorder-to-test evidence flow</summary>

```mermaid
sequenceDiagram
    participant User
    participant MCP as shaft-mcp
    participant Recorder as Web/Appium recorder
    participant Capture as shaft-capture session JSON
    participant Codegen as replay/code blocks
    participant Test as SHAFT test
    participant Trace as Doctor/Heal/Trace

    User->>MCP: capture_start / mobile_record_start / playwright_record_start
    MCP->>Recorder: inject overlay or bind Appium/Playwright events
    Recorder->>Capture: actions, checkpoints, context, network, privacy
    User->>MCP: capture_stop / mobile_record_stop
    MCP->>Codegen: generate replay + review blocks
    Codegen->>Test: paste or insert SHAFT TestNG code
    Test->>Trace: Allure, trace, locator health, failure brief
    Trace->>MCP: doctor/healer recommendations
```

</details>

<details>
<summary>Agent URL-intent workflow</summary>

```mermaid
sequenceDiagram
    participant Agent
    participant MCP as shaft-mcp
    participant Browser as SHAFT WebDriver
    participant DOM as DOM + LocatorRanker
    participant Codegen as Capture/code blocks
    participant Failure as Allure/Trace/Heal

    Agent->>MCP: shaft_guide_search(intent)
    Agent->>MCP: driver_initialize()
    Agent->>MCP: browser_open_intent(targetUrl, userIntent)
    MCP->>Browser: navigate + read page source
    MCP->>DOM: parse actionable DOM and score locators
    DOM-->>Agent: bounded DOM, best locator, alternatives, SHAFT locator code
    Agent->>MCP: element_click / element_type / natural_act
    Agent->>MCP: capture_start, capture_stop, capture_code_blocks
    Agent->>Failure: doctor_analyze_failed_allure, trace_summarize, healer_run_failed_test
```

</details>

<details>
<summary>MCP tool family map</summary>

```mermaid
flowchart TB
    A[134 registered MCP tools] --> B[WebDriver browser + element]
    A --> C[Playwright browser + element + semantic]
    A --> D[Capture start/status/stop/generate]
    A --> E[Mobile native, web emulation, inspector, screenshots]
    A --> F[Doctor, Heal, Trace]
    A --> G[Guide search, scenario catalog, code guardrails, natural_act]
```

</details>

## Fresh Capture Commands

```powershell
# Build the current remote/main code used by the MCP server.
mvn -pl shaft-mcp -am package '-DskipTests' '-Dallure.automaticallyOpen=false' '-Dallure.open=false'
mvn -pl shaft-mcp dependency:copy-dependencies '-DincludeScope=runtime' '-Dallure.automaticallyOpen=false' '-Dallure.open=false'

# Validate browser_open_intent and the synchronized MCP tool manifest.
mvn -pl shaft-mcp -am '-Dtest=McpServiceHelperTest,ShaftMcpApplicationTests#contextRegistersExpectedLeanMcpToolApi' '-Dallure.automaticallyOpen=false' '-Dallure.open=false' test

# Start the shaft-mcp HTTP server from the merged build.
java -cp "shaft-mcp\target\shaft-mcp-10.2.20260623.jar;shaft-mcp\target\dependency\*" com.shaft.mcp.ShaftMcpApplication --spring.profiles.active=http --server.port=8093
```

```powershell
# Install Android SDK pieces used for the fresh emulator screenshot.
C:\Users\Mohab\android-tools\cmdline-tools\latest\bin\sdkmanager.bat --sdk_root=C:\Users\Mohab\android-tools `
  'platform-tools' 'emulator' 'platforms;android-36' 'system-images;android-36;google_apis;x86_64'

# Appium tool cache versions used by current SHAFT internal defaults.
appium@3.5.2
appium-uiautomator2-driver@7.6.2
appium-inspector-plugin@2026.5.1
```
