# SHAFT Modular-Era Feature Catalog

SHAFT is now a modular Java 25 automation framework with a lean core, opt-in power modules, and evidence-first tooling for browser, mobile, API, Playwright, MCP, IntelliJ IDEA, SikuliX desktop image automation, capture, reporting, diagnosis, and healing.

This catalog is written for framework users who want to know what they can adopt
now, and which command or API gets them started.

For the release-by-release history behind these capabilities, see the
[GitHub releases](https://github.com/ShaftHQ/SHAFT_ENGINE/releases). For
installation, configuration, and full API reference, see the
[user guide](https://shafthq.github.io/docs/start/overview).

Screenshots in this catalog are real repository evidence under
`shaft-engine/src/main/resources/modular-era-feature-catalog/`, shown beside the
feature each one proves.

## Start Here

| If you need to... | Start with... | Why it matters |
| --- | --- | --- |
| Upgrade without pulling every optional integration into the core artifact. | [Modular adoption](#modular-adoption) | `shaft-engine` stays lean while BrowserStack, visual, video, AI, Doctor, Heal, and Capture remain available as opt-in modules. |
| Let an agent inspect pages, choose locators, record flows, and review generated code. | [MCP and agent workflows](#mcp-and-agent-workflows) | `shaft-mcp` exposes WebDriver, Playwright, mobile, capture, Doctor, Heal, Trace, guide search, and guardrail tools through one automation surface. |
| Use SHAFT workflows inside IntelliJ IDEA. | [IntelliJ IDEA plugin](#intellij-idea-plugin) | The stable plugin is the front door for coding-partner work: Assistant and Guided collect intent, current source, selected text, and evidence before MCP plans reuse and verification. |
| Turn exploratory browser or mobile sessions into maintainable Java tests. | [Capture and code generation](#capture-and-code-generation) | Recorder sessions preserve actions, checkpoints, locators, context, privacy, and replay snippets. |
| Make Android/Appium setup and recording less coordinate-driven. | [Mobile automation](#mobile-automation) | Toolchain diagnostics and locator-first Inspector recording show the exact device, locator, and fallback state. |
| Debug failed tests from evidence instead of guessing. | [Doctor, Heal, Trace, and reporting](#doctor-heal-trace-and-reporting) | Failure briefs, traces, locator health, healing decisions, and report UI give a shorter path from failure to fix. |

## What Changed

| Area | What you can adopt now | Proof |
| --- | --- | --- |
| Lean modular core | Adopt the core engine first, then add BrowserStack, visual, video, SikuliX desktop image automation, AI, Doctor, Heal, Capture, or MCP only when a project needs them. | `module-map.png` |
| MCP automation surface | Drive WebDriver, Playwright, mobile, recording, coding-partner planning, guide search, generated-code review, evidence manifests, and failure triage through one local server. | `mcp-tools.png` |
| IntelliJ IDEA plugin | First-run setup is a four-step click-through (pick agent, copy command, run in terminal, check setup). Assistant and Guided then plan repository-aware work from intent, current source, selected code, and evidence before codegen starts. | `intellij-plugin-mcp-setup.png`, `intellij-plugin-assistant.png` |
| Recorder-to-code workflow | Capture real user actions, preserve edited step intent and checkpoints, plan reuse with `shaft_coding_partner_plan`, then generate TestNG replay snippets, review blocks, patch previews, evidence manifests, and Page Object insertions. | `web-recorder.png`, `capture-catalog.png` |
| Locator-first mobile recording | Resolve Appium Inspector pointer gestures through the accessibility tree, then generate ranked locator inventories, Page Object drafts, and record-at-target snippets before using coordinate fallback. | `android-recorder-working.png`, `android-recorder-locator-details.png` |
| Evidence-led failure work | Combine Allure failure briefs, traces, locator health, healing reports, and optional reviewed AI advice. | `doctor-heal-trace.png`, `api-reporting.png` |
| SikuliX desktop automation | `shaft-sikulix` owns the `com.sikulix:sikulixapi` dependency, keeping image-based desktop flows out of the lean engine artifact. | `module-map.png` |

Release-by-release detail, including the issue numbers behind each change, lives
in the [GitHub releases](https://github.com/ShaftHQ/SHAFT_ENGINE/releases).

## Generated-Code Locator Policy

Every SHAFT code generator — `capture_target_candidates`, `capture_pick_locator`,
`test_plan_explore`, `capture_code_blocks`, and `capture_generate_replay` — picks
locators in strict tier order:

1. A unique, stable, author-written id: `hasAnyTagName().hasId(...)`.
2. An ARIA-role locator: `hasRole(...)`.
3. A self-verified, non-absolute `By.xpath(...)`, and only when the element has
   neither of the above.

Anything else is refused rather than emitted. Generators never produce
`SHAFT.GUI.Locator.xpath(...)`, the raw `id`/`name`/`cssSelector`/`className`/
`tagName(...)` factories, or a Smart Locator (`inputField`/`clickableField`).

## Coding Partner Workflow

The recorder/codegen handoff is a review loop, not a one-shot generator: plan the
working set and user steps, reuse the recommended Java owner and insertion
anchor, avoid duplicate locators/actions/classes, inspect the patch preview,
collect evidence, then verify locally.

The public entry point is IntelliJ, not raw MCP: Assistant `/partner` and Guided
`Plan coding partner` gather the IDE context, then MCP returns the reuse plan,
reviewed code blocks, and a focused verification command.

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

| Step | Tool | What you get |
| --- | --- | --- |
| Plan | `shaft_coding_partner_plan` | `workingSetSummary`, a `stepPlan`, `reuseMatches` versus `missingCodeItems`, plus `recommendedTargetSourcePath` and `recommendedInsertionAnchor`, so agents extend existing owners instead of creating new page objects. |
| Preview | `shaft_coding_partner_diff` | A unified diff against an existing Java target and anchor, without writing files. |
| Guard | `test_code_guardrails_check` | Runs SHAFT guardrails over generated code before it is applied. |
| Verify | `verify_run_focused` | Runs the plan's smallest Maven verification command headlessly and returns a bounded pass/fail summary. |
| Triage | `Fix Failing Test` | Routes Allure/trace evidence and locator source back into the plan, closing the analyze, plan, verify loop. |

The Assistant shows five core commands by default — `/record-web`,
`/record-mobile`, `/codegen`, `/doctor`, and `/upgrade`. Everything else,
including `/verify` and `/skills`, sits behind Expert mode. Natural-language
intents work in both modes.

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

### Assistant browser control

IntelliJ Assistant chat routes browser-control requests to sequenced SHAFT MCP
tools. WebDriver is the default for natural prompts such as
`open https://example.com and inspect the sign in link`: the generated sequence
calls `driver_initialize` before `browser_open_intent`, keeps DOM output
bounded, and returns locator candidates with evidence. Playwright is used only
when a prompt asks for it explicitly.

Browser-control requests can inspect DOM, title, URL, screenshots, navigation,
window state, and session cleanup. Screenshot requests write workspace evidence
files and omit base64 by default.

### Workflows

| Workflow | What it gives users | Entry point |
| --- | --- | --- |
| Install and run | Local installers for Codex, Claude, Claude Desktop, Copilot, Copilot IntelliJ, plus installer defaults that the IntelliJ plugin can use to find the generated stdio argfile automatically. The Marketplace plugin itself does not run installer scripts. | `py -3 scripts/mcp/install_shaft_mcp.py --client intellij-plugin --json` |
| URL intent orientation | Open a URL, bound the DOM, rank actionable elements, return SHAFT locator code, and suggest the next MCP tools. | `driver_initialize -> browser_open_intent(targetUrl, userIntent, 200000, 10)` |
| Coding partner plan | Summarize the current IntelliJ/user intent, rank existing Java targets, return a structured `stepPlan`, recommend a target source/anchor, list missing code, suggest MCP proof calls, and return a focused verification command. | `shaft_coding_partner_plan(repositoryPath=".", intent="login", currentSourcePath="src/test/java/...")` |
| Locator inspection | Reuse `shaft-capture` `LocatorRanker` scoring for role, accessible name, label, test id, id, name, CSS, and XPath alternatives. | `bestLocator.strategy=ROLE; shaftLocatorCode=SHAFT.GUI.Locator.hasRole(Role.BUTTON).hasText("Sign in").build()` |
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
orientation.elements[0].shaftLocatorCode=SHAFT.GUI.Locator.hasRole(Role.BUTTON).hasText("Sign in").build()
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

`shaft-intellij` is the stable IntelliJ IDEA plugin (`io.github.shafthq.shaft`, `10.3.20260801`). It is the public front door for coding-partner work: start in Assistant or Guided, let MCP plan reuse, review generated code blocks, and run the focused verification command. It is intentionally thin: first-run setup defaults to Codex CLI, walks through `Pick agent`, `Copy command`, `Run in terminal`, and `Check setup`, then uses installer defaults to find and persist the local SHAFT MCP launch command automatically before revealing `Start chatting`. The plugin does not download or execute installer scripts at runtime. Settings remain available later for Local/Cloud routing, API keys, and custom local MCP commands.

| Surface | What users get | Entry point |
| --- | --- | --- |
| Tool window | Right-side SHAFT assistant panel with dedicated workflow tabs for Assistant, Guided, Recorder, Inspector, Triage, Evidence Tools, Projects, and Advanced Tools. | `Tools -> SHAFT -> Open SHAFT` |
| First-run setup | If a tested MCP command has passed setup, the tool window opens the Assistant directly. Otherwise, setup opens with `Connect SHAFT Assistant`, defaults to Codex CLI, and shows a simple stepper: `1 Pick agent`, `2 Copy command`, `3 Run in terminal`, `4 Check setup`, then `Ready`. Users never see or paste the managed stdio command; `Check setup` discovers it from the installer output paths, verifies the selected local agent, and reveals `Start chatting` on success. Retry actions stay enabled after failures, with categorized diagnostics that avoid exposing the managed command. | `Tools -> SHAFT -> Open SHAFT` |
| Settings and providers | Retest MCP, change Local/Cloud routing, select the cloud provider/model, store or clear OpenAI, Anthropic, Gemini, and GitHub keys, and opt in to a project-wide Test Execution override (browser/headless) written to `custom.properties`. Only the selected cloud provider key is passed to MCP. | `Settings -> SHAFT` |
| First-run setup: Connect agent | Once SHAFT MCP verifies but the selected agent CLI's readiness check hasn't succeeded, a `Connect agent` button next to `Start without an agent` retries just the agent check from the Ready step -- no need to scroll back through earlier steps and rerun the whole probe. | `Tools -> SHAFT -> Open SHAFT` |
| Guided workflows | Pick starter templates for browser capture to Page Objects, failed Allure analysis, Selenium-to-SHAFT conversion, confirmed new SHAFT projects, and locator inspection without editing JSON first; the recorder action now says `Review code` because it returns reviewed SHAFT code blocks. Browser (Chrome/Edge) and Headless are always-visible primary controls, not buried behind Advanced options, with a visible hint when a team `.shaft/recorder-policy.json` locks Headless. | `Guided` tab |
| Recorder | A curated Quick Start (Target URL, Browser, Headless, Output path, Start/Stop/Check status/Review code) for WebDriver recording drives `capture_start`/`capture_stop`/`capture_status`/`capture_code_blocks` directly; every other Recorder tool (target discovery, record-at-target patch preview, backend comparison, evidence packs, checkpoints, replay generation, Playwright replay, mobile replay) stays one click away behind "Advanced: all Recorder tools (raw JSON)". | `Recorder` tab |
| Project tools | Create a SHAFT project after confirmation, preview an upgrade, or apply an approved upgrade through MCP. | `Projects` tab |
| Triage | Prepare failed Allure analysis, trace analysis, Doctor fix suggestions, Healer runs, and locator proposal requests. | `Triage` tab |
| Evidence and healing | Run failed-test analysis, trace lookup, trace analysis, and healer templates from the IDE. | `Evidence Tools` tab |
| Mobile Inspector | Check mobile tooling, prepare Inspector recording, inspect status, read accessibility trees, and take mobile screenshots. | `Inspector` tab |
| Agent helpers | Switch the Assistant composer between Ask, Plan, and guarded Agent prompts through local Codex, Claude Code, or Copilot CLI; Ask/Plan can route to OpenAI, Anthropic, Gemini, or GitHub Models with a stored key; Cloud Agent is demoted to Plan because provider chat cannot mutate the local workspace; project chats persist rendered messages without raw payloads; code writing and conversion prompts are scoped to the current editor file; the compact `Codex CLI` label exposes `Agent: Local / Codex / CLI` as a tooltip; the command picker shows summaries/examples and the context button exposes `@` and `#` workflow/project context without filling empty chats with starter text; action chrome stays hidden until useful; tool-call milestones and run outcomes render as their own chat bubbles instead of a separate timeline (issue #3695); Assistant status text is exposed to assistive technologies. | `Assistant` tab |
| Record at target | Open the side panel, prefill `capture_record_at_target_code_blocks` from the Java caret context, then review code blocks, inspect the patch preview, apply intentionally, and verify. | `Recorder` tab |
| Editor action | Start from the current Java caret context through the `Record SHAFT Flow Here` action in Tools or the editor popup menu; it now uses `recordings/intellij-capture.json` instead of a placeholder session path. | `Record SHAFT Flow Here` |
| Run-configuration overrides | A "SHAFT" tab on IntelliJ's native TestNG/JUnit run-configuration editor: opt in per run, then override `targetBrowserName`, `headlessExecution`, and extra `-D` VM args for that one run without touching `custom.properties`. Untouched by default, so every existing run configuration keeps inheriting the project file. | `Run/Debug Configurations -> SHAFT` |
| SHAFT Tests discovery tree | A real package/class/@Test-method tree, discovered from PSI project-wide, so tests that have never run are still visible -- not just ones with recorded history. Recorded pass/fail/last-run-time layers on as a decoration where a matching run exists; nodes with no match show as "not yet run" instead of being hidden. Refresh re-scans and reloads; Clear resets only the run-history decorations, the discovered structure stays. | `SHAFT Tests` tab |

The plugin rides as a right-side IntelliJ panel, similar to assistant tools such as GitHub Copilot. The Assistant keeps Ask/Plan/Agent mode switching in the bottom composer, gates source mutation behind the local Agent approval checkbox, restores project chat sessions without persisting raw MCP payloads, scrolls newly sent prompts into view before long-running responses finish, uses compact JetBrains-style icons for dense controls including Copy all, Clear, and Rerun, turns the submit control into an animated spinner with hover-to-cancel while running, changes Cancel into Kill after the first cancellation request so a second click terminates the active process immediately, clears the running state when a local Agent result arrives, keeps code blocks visually distinct from chat bubbles with a light editor-style palette in light mode and a dark surface in dark mode, anchors Send at the bottom-right with Ctrl+Enter, Command+Enter, and Ctrl+click shortcuts, lets Escape cancel a running request from the Assistant view, keeps empty chats focused on the composer, renders command summaries/examples in the picker, exposes `@workflow` and `#project` context through a dedicated context button, and exposes the selected local agent as compact text such as `Codex CLI` with `Agent: Local / Codex / CLI` in the tooltip. Tool-call milestones, streamed progress updates, and terminal run outcomes (Completed/Failed/Cancelled/Killed) render as their own chat bubbles in the transcript rather than a separate "Run timeline" list (issue #3695); action chrome (Copy, Clear, Rerun, Cancel) still stays hidden until a prompt, tool run, approval, completion, cancellation, or failure gives it useful state. The Advanced Tools and Recorder tabs still show exact JSON arguments, validate them before run, require the same verified MCP setup state as the Assistant, and write formatted outputs to an output pane.

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
    <td colspan="2">Screenshots show the 860 px right-side panel, 360 px narrow Assistant and setup views, first-run MCP installer/default/success/error states, the empty Assistant composer, live assistant output with agent-milestone chat bubbles, Guided starter templates, command hint affordance, and SHAFT settings panel captured on the current plugin branch in standard IntelliJ light and dark themes.</td>
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
Generated replay syntax: driver.element().click(SHAFT.GUI.Locator.hasRole(Role.TEXTBOX).hasAttribute("name", "Username").build());
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
| Locator-first replay | Convert a tap into `ACCESSIBILITY_ID` and a swipe target into `ID` when the accessibility tree supports it. | `capture_code_blocks --session target/shaft-evidence/mobile-inspector-locators.json --backend mobile` |
| Mobile Page Object handoff | Return replay, ranked locator inventory, action sequence, Page Object draft, and focused insertion snippets for existing mobile POM classes. | `capture_code_blocks -> capture_record_at_target_code_blocks` |

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
    A[90 registered MCP tools] --> B[WebDriver browser + element]
    A --> C[Playwright browser + element + semantic]
    A --> D[Capture start/status/stop/generate]
    A --> E[Mobile native, web emulation, inspector, screenshots]
    A --> F[Doctor, Heal, Trace]
    A --> G[Guide search, scenario catalog, code guardrails, natural_act]
```

</details>
