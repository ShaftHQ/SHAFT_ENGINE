# SHAFT IntelliJ plugin — Chaos complementary revamp input

**Author:** ChaosEngine executor  
**Date:** 2026-10-29 (Africa/Cairo, UTC+3)  
**Branch tip audited:** `ShaftHQ/SHAFT_ENGINE` `main` @ `6ffde6cdc` (pluginVersion `10.4.20261029`)  
**retrieve:** used (graphify → `shaft-skills/ce-pack/references/shaft-mastery/intellij-plugin.md`, `tools/intellij-plugin-recording/README.md`, `ShaftToolWindowPanel`)

## Relationship to Coach's audit

Do **not** treat this file as a standalone architecture brief.

Coach already published the independent baseline audit (private working repo; no client data copied here):

- **Repo:** `MohabMohie/automatest-coach-workspace` (private)
- **Branch:** `coach/intellij-plugin-revamp-input` @ `ee3af69`
- **Path:** `coach/intellij-plugin-revamp/REVAMP-INPUT.md`

That document owns: location/Marketplace overview, package map, feature list, open/closed issue summary, JetBrains-guidelines gap table, and the phased revamp proposal (Phases 0–4).

This file only adds **verified deltas** Chaos collected while preparing the public tracking epic: dead IA code, reliability lessons from mastery docs, publish-pipeline detail, headless-verify attempt on the shared box, and a concrete IntelliJ UI Test Robot / IDE Starter recommendation Coach left as “scripted journeys” without naming the Platform tooling.

---

## 1. Dead three-stage IA (code still in tree)

Epic [#5942](https://github.com/ShaftHQ/SHAFT_ENGINE/issues/5942) (three-stage Design | Automation | Reporting tool window) is **closed**, and `AgentFirstUxTest` asserts the live selector is only **Agent / Workflow / Execution log**, with “Analysis & Design”, “Reporting & Analytics”, “Visual Baselines”, and “Locator playground” removed from selectable surfaces.

Yet the stage panel classes remain and are **never constructed**:

| Class | LOC | Instantiation in `ShaftToolWindowPanel` |
|---|---|---|
| `DesignStagePanel` | 592 | field always set to `null` (e.g. lines ~166, ~204) |
| `AutomationStagePanel` | 168 | same |
| `ReportingStagePanel` | 446 | same |

≈ **1.2k LOC** of stage UI plus accessors (`designStagePanel()`, routing that still names `SURFACE_WORKFLOW` / Automation tab constants) sit beside the Agent-first shell. Routing helpers still reference `AutomationStagePanel.LIVE_RECORD_TAB` for persistence.

**Revamp implication:** Phase 1 must either delete the orphaned stage stack or finish a single IA (three-stage *or* Agent-first), not leave both. Coach’s “expert mode still exposes deep MCP catalogs” gap is real; this adds **concrete dead code** that inflates cognitive load for maintainers and risks half-wired navigation.

---

## 2. Test strategy gap — name the Platform robots

Coach correctly notes mostly unit/fake Swing tests plus a few `BasePlatformTestCase` / `LightJavaCodeInsightFixtureTestCase` fixtures, and that #6425 closed with “IDE Starter kept for later.”

Chaos verified:

- **No** `RemoteRobot`, `com.intellij.remoterobot`, or `ide-starter` dependency in `shaft-intellij` sources or Gradle build.
- Comments in several tests explicitly say the module has **no** live `BasePlatformTestCase` Application for many panels (`PickLocatorAtCaretActionTest`, `ShaftTestWatchServiceTest`, `AssistantTranscriptViewTest`).
- UI evidence today is `ShaftPluginScreenshotRendererTest` + headed `tools/intellij-plugin-recording/` scripts — Playwright cannot see Swing (mastery doc).

**Recommend for the epic (maps to Coach Phase 0 + 3):**

1. Keep fast JUnit 5 panel/unit tests as the PR gate.
2. Add **IntelliJ UI Test Robot** (RemoteRobot) *or* **IDE Starter** smoke jobs on a schedule / `workflow_dispatch`: Marketplace ZIP → cold IDEA → install → open SHAFT tool window → complete first-run happy path assertions.
3. Keep screenshot regeneration **scoped** (`--tests ShaftPluginScreenshotRendererTest` only). Mastery doc + #3782/#3786: full-suite + screenshotDir still breaks tree-selection tests via sticky L&F / `DefaultTreeUI` latch even with `LookAndFeelIsolationExtension`.
4. Do not weaken existing tests to green a rewrite.

---

## 3. Headless build/test attempt on this box (≤5 GiB budget)

Task asked to verify by building/running tests headless within ≤5 GiB RAM.

| Check | Result (2026-10-29 ~19:27 Africa/Cairo) |
|---|---|
| Free / available RAM | ≈ **1.1 GiB available** of 15 GiB; **0 swap**; box shared by many agents |
| `java` on PATH | **missing** (JDKs exist under `/home/box/jdk25`, `/home/box/tools/jdk-25.*`, `/home/box/jdk21` but not selected) |
| Full `./shaft-intellij/gradlew check` | **Not run** — IntelliJ Platform download + test JVM would exceed the ≤5 GiB local-build ceiling and starve the shared box |
| Code/issue/Marketplace verification | Done (this doc + Coach) |

**Revamp implication:** journey evidence for Phase 0 should run on a dedicated CI/machine profile (or owner machine), not assume the shared agent box can boot `runIde`. Document required RAM (≥8 GiB free recommended for Platform tests) in the epic.

---

## 4. Reliability / threading lessons (mastery) not in Coach’s table

From `shaft-skills/ce-pack/references/shaft-mastery/intellij-plugin.md` — fold into Phase 1–3 acceptance so the rewrite does not reintroduce known failures:

| Lesson | Issue / PR | Revamp rule |
|---|---|---|
| MCP `isError: true` inside a successful JSON-RPC envelope is a **failure** — never surface as Finished/OK | PR #3396 | Keep fail-closed tool-result handling in any Assistant rewrite |
| Capture `this::method` as a **field** before `add`/`remove` listener pairs | #3621 | Presenter/lifecycle split must preserve identity |
| `StatusText` / `getEmptyText()` clips ~42 chars and does not wrap | mastery | Prefer `PlaceholderTextArea` / manual paint for empty states in UI DSL migration |
| Long-lived pollers spawning MCP every 30s are a leak | mastery | Lifecycle-scope to tool-window `Disposable` |
| Setup badges only from real verification, never “user clicked” | PR #3427 | Wizard rewrite must keep |
| Recordings must not belong to an ephemeral agent turn | PR #3431 | Service owned by tool window |
| Grok (and other) assistant sessions dying silently | closed [#6748](https://github.com/ShaftHQ/SHAFT_ENGINE/issues/6748) | Reattach / exit-reason UX is still a journey acceptance item even though the issue is closed |

Build ceiling history: #3784 JDK 21 temporary ceiling after Windows G1 crash; #4779 raised daemon to JDK 25 after verified builds — keep Gradle wrapper coupling (`shaft-intellij/gradlew` only).

---

## 5. Publish / Marketplace pipeline detail

Coach lists the publish workflow. Additions:

| Piece | Detail |
|---|---|
| Trigger | `publish-intellij-plugin.yml` is `workflow_dispatch`; **also** embedded as a job in `mavenCentral_cd.yml` (`Publish IntelliJ plugin`, concurrency group `publish-intellij-plugin`) |
| Verify vs publish | Separate jobs; Marketplace version is **single-use** — never wrap upload in verify retry |
| Fail-closed | `notify-missed-publish` + label `intellij-marketplace-publish-miss`; receipt script `scripts/ci/assert_intellij_marketplace_receipt.py` (#5221/#5227/#5344) — Gradle `BUILD SUCCESSFUL` is the receipt; listing lag is advisory |
| Recent standalone publish runs | Several **failures/cancels** in Aug 2026 (`10.3.20260820`, `10.3.20260824`, …); last listed success `10.3.20260817` in that workflow’s short history — release path still needs babysitting |
| EAP nightly | `intellij-eap-verify.yml` — **success** on recent Oct 25–29 2026 runs |
| Marketplace versions (API) | Latest `10.4.20261029` approved, sinceBuild `243.0`, **0** downloads yet on that build; older `10.4.*` builds hold the download mass; plugin total ≈900 |
| Descriptor | `require-restart="true"` on root `plugin.xml` — any UI shell change costs a full IDE restart for users |

**Monorepo build coupling:** `processResources` generates the property catalog from `../shaft-engine/.../properties/internal` and copies `shaft-mcp` / `shaft-cli` tool/command indexes. Sparse/plugin-only checkouts cannot `check` without those trees. Revamp CI must keep monorepo context.

---

## 6. What works vs broken (Chaos judgment, additive)

Coach already framed owner feedback as the acceptance bar. Evidence-backed split for the epic checklist:

### Works (keep)

- Marketplace listing + signed publish path exist; EAP verifier green recently.
- Editor intelligence from closed #6432 (properties completion/inspections, live templates, Kotlin gutter, Gradle overrides).
- First-run wizard structure and copy-installer model (#6784 family).
- Strict Plugin Verifier failure levels and `-Werror` deprecation on compile.
- Project gating (`ShaftProjectDetector`) so the plugin stays inert outside SHAFT projects.
- Large automated unit/UI suite (~171 test files) — valuable once megaclasses shrink.

### Broken / debt for humans (prioritize)

- **IA schizophrenia:** Agent-first live UI + orphaned three-stage panels + advanced/expert MCP catalogs.
- **Megaclass UIs:** Assistant / MCP setup / GuidedWorkflow (Coach sizes) — unmaintainable and the main “looks bad / feels poorly made” surface.
- **No New Project wizard** (Coach) and **no RemoteRobot/IDE Starter** smoke of the Marketplace ZIP.
- **Session reliability** (#6748 class of bugs) still a journey risk.
- **Shared-box cannot prove headless green** under ≤5 GiB today — do not claim local green without a machine that can boot the Platform.
- Coverage still open (#7080 / #7018) but is **not** the primary human-usability gap.

---

## 7. Recommended epic phase checklist (links Coach; does not replace)

Use Coach Phases 0–4 as the product narrative. Chaos delivery checklist additions:

- [ ] **P0a** Link Coach audit + this complementary doc on the public epic; no client data in SHAFT_ENGINE.
- [ ] **P0b** Delete or quarantine orphaned `*StagePanel` code **or** restore one coherent IA (decision recorded).
- [ ] **P0c** Script 5 Coach journeys on a machine with ≥8 GiB free; file defects as `subsystem:intellij` children.
- [ ] **P1** Kotlin UI DSL v2 shell + presenter split (Coach); preserve mastery reliability rules in §4.
- [ ] **P2** Human workflows (Coach); RemoteRobot/IDE Starter smoke green on Marketplace ZIP.
- [ ] **P3** Publish babysit + receipt asserts; coverage #7080 without weakening tests.
- [ ] **P4** Power features only behind progressive disclosure (Coach).

Out of scope remains Coach’s list (no shaft-mcp rewrite, no non-IntelliJ IDEs, no fluent API redesign).

---

## 8. Evidence appendix (Chaos-only)

- Worktree: `/workspace/wt-intellij-revamp-input` sparse checkout on `ChaosEngine/intellij-plugin-revamp-input`
- Orphan nulling: `shaft-intellij/src/main/java/com/shaft/intellij/ui/ShaftToolWindowPanel.java` (`designStagePanel = null`)
- Agent-first contract: `shaft-intellij/src/test/java/com/shaft/intellij/ui/AgentFirstUxTest.java`
- Mastery: `shaft-skills/ce-pack/references/shaft-mastery/intellij-plugin.md`
- Publish: `.github/workflows/publish-intellij-plugin.yml`, `mavenCentral_cd.yml` IntelliJ job, `scripts/ci/assert_intellij_marketplace_receipt.py`
- Marketplace updates API: plugin `32529` versions `10.4.20261029` …
- Coach baseline: private `MohabMohie/automatest-coach-workspace` @ `ee3af69` → `coach/intellij-plugin-revamp/REVAMP-INPUT.md`
