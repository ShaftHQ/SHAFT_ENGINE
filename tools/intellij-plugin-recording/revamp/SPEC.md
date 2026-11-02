# SHAFT IntelliJ plugin revamp — consolidated phased spec

**Status:** Delivery SPEC under epic [#7568](https://github.com/ShaftHQ/SHAFT_ENGINE/issues/7568)  
**Author:** ChaosEngine (public consolidation from Coach SPEC)  
**Date:** 2026-10-29 (Africa/Cairo, UTC+3)  
**Plugin:** `shaft-intellij` in `ShaftHQ/SHAFT_ENGINE` · id `io.github.shafthq.shaft` · Marketplace [32529-shaft](https://plugins.jetbrains.com/plugin/32529-shaft)  
**Owner bar (2026-10-29):** not usable via the real IDE workflow; looks bad; feels poorly made; does not function as it should in most cases. **Acceptance = human journey success from a Marketplace ZIP**, not feature-checklist parity.

`retrieve: used` (private coach workspace graphify; low plugin-hit relevance — consolidation driven by input docs below).

---

## 0. Inputs (locked versions)

| # | Source | Where (public pointers only) | Role |
|---|---|---|---|
| 1 | Coach independent audit | Private coach workspace · branch `coach/intellij-plugin-revamp-input` @ `ee3af69` (path withheld) | Architecture, features, Marketplace, JetBrains gaps, Phases 0–4 narrative |
| 2 | Chaos complementary | [`docs/revamp/input.md`](https://github.com/ShaftHQ/SHAFT_ENGINE/blob/ChaosEngine/intellij-plugin-revamp-input/docs/revamp/input.md) on `ChaosEngine/intellij-plugin-revamp-input` @ `db4319e` | Orphaned stage panels, mastery reliability rules, publish pipeline, UI Test Robot / IDE Starter, shared-box limits |
| 3 | Real-user mobile + API feedback | Private coach workspace · branch `coach/intellij-plugin-revamp-input-hrsd` @ `442d471` (path withheld) | Platform switching, cloud app ids, secrets, Allure merge, locator/inspector, API builder — prioritized F1–F16 |
| 4 | Carl UX/UI pack | Private designer workspace · branch `design-shaft-intellij-audit` @ `ebeefe0` (path withheld) | 21 findings, design-system principles (mocks not binding — see 4a) |
| 4a | Coach design acceptance | Coach SPEC branch `coach/intellij-plugin-revamp-spec` @ `efbed24` (review note withheld) | **SIGN** — Carl pack r2 @ `eef3c90`; Phase 1 implements mocks + DESIGN-SYSTEM + RUN-TAB-WIRE (public UI only; no private pack dump) |
| 5 | Tracking epic | [#7568](https://github.com/ShaftHQ/SHAFT_ENGINE/issues/7568) | Delivery checklist; child issues under `subsystem:intellij` |

No client, trainee, or pricing data in UI strings, issues, Marketplace copy, or child tickets. No personal names in public docs.

---

## 1. Problem statement (reconciled)

~52k LOC Java Swing shell over `shaft-mcp`; Platform Gradle Plugin 2.x and verifier discipline are strong; editor intelligence and first-run structure already shipped (#6432, #6784). Human failure mode is **workflow cohesion + density + reliability + New UI chrome**, not “missing Playwright checkboxes.”

Concrete debt all four inputs agree on:

- Megaclass UIs (`ShaftAssistantPanel`, `ShaftMcpSetupPanel`, `GuidedWorkflowPanel`, settings).
- No New Project / Module wizard.
- Agent-first live IA plus **orphaned** three-stage panels (~1.2k LOC) plus expert MCP catalogs.
- PNG-in-SVG tool-window / plugin icons; light-only color fallbacks; emoji status; zero `JBFont` / almost no Kotlin UI DSL.
- No RemoteRobot / IDE Starter smoke of the Marketplace ZIP.
- Mobile/API users still edit properties by hand for platform/cloud switches; secrets and Allure merge are tribal.

---

## 2. Conflicts and decisions

| ID | Conflict | Decision (Coach) | Check |
|---|---|---|---|
| C1 | Live IA: Agent / Workflow / Execution log (Coach/Chaos) vs Carl mock tabs Assistant / Tests / Setup vs DESIGN-SYSTEM “Design / Automation / Reporting” | **Agent-first product IA.** Human labels may be Assistant / Tests / Setup if journey scorecard prefers them, mapped 1:1 — **not** restoring three-stage panels. Delete or quarantine orphaned `*StagePanel`. Carl must drop three-stage wording (design REJECT B2). | P0b child issue; Carl SIGN r2 landed |
| C2 | Coach Phase 2 run tab (browser/env + Allure) vs field F1 (Platform × Execution × Device × App picker) | **Phase 2a** = Coach web/Allure + schema properties editor (field F2 subset). **Phase 2b** = field F1 target picker + session awareness (F5) on same UI DSL form. | Separate child issues; 2b not blocked on 2a paint |
| C3 | Coverage #7080 vs human usability | Coverage is **Phase 3 secondary**; do not weaken tests to green a rewrite. Human journeys are the bar. | Epic checklist |
| C4 | Kotlin UI DSL needs Kotlin; module is Java-only | New Settings / first-run / run-tab / chrome modules in **Kotlin**; no big-bang rewrite of Assistant transcript. | Phase 1 work items |
| C5 | Carl mocks vs Carl F01 (filled blue “S” strip icon) | **SIGN r2** — implement signed mocks; F01 greys + vector strip icons still bind. | Coach review @ `efbed24` |
| C6 | Power/MCP expert catalogs vs progressive disclosure | Expert MCP / Design→Automation / flake analytics only after P0–P2 journeys pass (Coach Phase 4). | Gate on journey scorecard |
| C7 | Shared agent box ≤5 GiB cannot boot `runIde` | Journey + Platform UI tests run on CI / dedicated machine (≥8 GiB free). Do not claim local green on the shared box. | Phase 0 / 2b notes |
| C8 | Field cloud-grid features vs “no shaft-mcp rewrite” | Prefer IDE UX over properties; protocol gaps filed only when a journey proves them. Cloud upload (F3) may need MCP/tooling — spike before committing. | Phase 2b spike |

**Open conflicts (unresolved until evidence):**

- **O1** — Exact human tab labels (Assistant/Tests/Setup vs Agent/Workflow/Execution): decide in Phase 0 journey scorecard walk; default mapping in C1.
- **O2** — **DECIDED (Chaos, 2026-10-29):** UI harness = **JetBrains IDE Starter + Driver** (current JetBrains-recommended Platform UI test stack). **Remote Robot** remains a fallback only if Starter cannot run headless in CI. Rationale: Starter is the supported JetBrains path for cold-IDE / Marketplace-ZIP style smoke, aligns with #6425 “IDE Starter kept for later,” and avoids a parallel RemotRobot dependency unless CI proves Starter insufficient. Epic W2.13 acceptance updated accordingly.
- **O3** — **DECIDED (2026-10-29):** Carl IntelliJ design pack **r2 SIGNED** (designer branch `design-shaft-intellij-audit` @ `eef3c90`; Coach review on `coach/intellij-plugin-revamp-spec` @ `efbed24`). Phase 1 visuals **unblocked** — implement from signed mocks + DESIGN-SYSTEM + RUN-TAB-WIRE. Do not copy private-only pack content into the public repo beyond what the UI needs; no client data.

---

## 3. Target user journeys (acceptance criteria)

Record each on **clean IntelliJ IDEA + Marketplace ZIP** (not a monorepo `runIde` only). Score with an extended `FirstRunUxScore`-style card (1–5: discover, understand, succeed, recover, feel native).

### J1 — Install + first-run (P0)

1. Install SHAFT from Marketplace; restart if `require-restart`.
2. Open a SHAFT Maven/TestNG (or Gradle) project; tool window appears only when `ShaftProjectDetector` says yes (`isApplicable`).
3. Complete first-run: facts → prereqs → agent recommendation → copy installer → Check → first success.
4. **AC:** MCP verified badge from **real** check (not click); Setup failure offers recovery verb; no expert mode required; no terminal tribal knowledge beyond the one installer command shown.

### J2 — New SHAFT project (P0)

1. Welcome / New → SHAFT test project (Maven required; Gradle optional).
2. JDK + Maven checks; optional “open Assistant after create.”
3. **AC:** Compiling empty test module; `custom.properties` + secrets `.example` present; tool window opens on first open.

### J3 — Record → keep code (P0)

1. Record web (and later mobile) flow from Tools / gutter / Assistant without expert mode.
2. Insert at caret / generate page object path is obvious.
3. **AC:** Generated code compiles; Pick Locator + match-count work; no deep MCP catalog required.

### J4 — Run → Allure / Doctor (P0)

1. One-click run of a single test/method (web path Phase 2a; mobile target picker Phase 2b).
2. On failure: first real error highlighted; evidence (screenshot/trace) one click; Doctor second step not first wall; Open Allure when CLI/report exists.
3. **AC:** Project files unchanged by overrides (`-D` / run config only); invalid platform combos blocked pre-launch (2b).

### J5 — Failed locator → Heal / Fix (P0)

1. From failure notification or Tests panel → Heal / Fix with Assistant.
2. **AC:** Plain-language failure if agent/MCP down; session reattach / exit reason visible (#6748 class); MCP `isError: true` never shown as OK.

### J6 — Mobile platform switch without property surgery (P0 for mobile track / Phase 2b)

1. Same test method → local Android / cloud Android / cloud iOS via target picker (field F1).
2. **AC:** Named run configs; secrets masked; cloud slot warning (F5); app alias from registry when F3 exists.

### J7 — API request → SHAFT code (P1 / Phase 4 or late Phase 2)

1. Builder → execute → generate `SHAFT.API` chain; secrets by key; prod target read-only for non-GET (field F10).
2. **AC:** Generated code compiles and reproduces response.

---

## 4. Phases and issue-sized work items

Priorities: **P0** ship-blocker for human journeys · **P1** clear guideline / workflow miss · **P2** polish / power.

Child issues: label `subsystem:intellij` + `agentic`; parent [#7568](https://github.com/ShaftHQ/SHAFT_ENGINE/issues/7568). One issue ≈ one PR-sized slice.

### Phase 0 — Spec, IA decision, journey scorecard

| ID | Priority | Work item | Acceptance |
|---|---|---|---|
| W0.1 | P0 | Land this SPEC + link on #7568; no client data in public repo | Epic body points at public Chaos input + private coach paths by branch/sha only |
| W0.2 | P0 | **IA decision recorded:** delete/quarantine `DesignStagePanel` / `AutomationStagePanel` / `ReportingStagePanel` **or** restore one IA — default **delete/quarantine** per C1 | `AgentFirstUxTest` green; dead classes gone or behind test-only fixture; no dual routing |
| W0.3 | P0 | Script/record J1–J5 on machine with ≥8 GiB free; file defects as children | Scorecard + screenshots/video attached to epic or wiki; defect list filed |
| W0.4 | P1 | UX scorecard template (extend FirstRunUxScore to full journeys) | Checked in under `shaft-intellij` docs or `tools/intellij-plugin-recording/` |
| W0.5 | P1 | Carl design resubmit → Coach SIGN | **DONE** — SIGN @ `efbed24` / pack `eef3c90`; mocks match F01 greys + C1 labels |

### Phase 1 — UI shell (native look + maintainability)

| ID | Priority | Work item | Acceptance |
|---|---|---|---|
| W1.1 | P0 | Vector tool-window + pluginIcon SVGs; ban PNG-in-SVG; size budget test (Carl F01) | New UI active recolor works; CI rejects `<image` / `data:image` in icon SVGs |
| W1.2 | P0 | `ShaftIconMappings.json` + `expui/` + `iconMapper` EP (F02); prefer `AllIcons` (F03); delete `ThemeIcon` (F04) | Classic + New UI resolve; generic actions use AllIcons |
| W1.3 | P0 | Dual-leg `ShaftUiColors` / status presentation; remove emoji status in Swing (F05, F07) | Dark+light status readable; no `new Color(0x…)` single-leg fallbacks |
| W1.4 | P1 | `JBFont` + `ShaftUiMetrics` pass on headers/settings/run/assistant chrome (F08, F09) | Appearance font scaling respected |
| W1.5 | P1 | Kotlin UI DSL Settings groups: Agent / MCP / Keys (F16); Password Safe unchanged | SearchableConfigurable ids preserved; layout matches signed mocks |
| W1.6 | P1 | Kotlin UI DSL run-config overrides tab (F14); short “Project default” label (F15) | Aligns with native Run pages |
| W1.7 | P1 | Tool window `isApplicable` + Open action visibility (F10); empty-state CTA pattern (F11, F20) | Hidden on non-SHAFT projects; empty Tests has primary action |
| W1.8 | P1 | Split facades/presenters for `ShaftAssistantPanel` / `ShaftMcpSetupPanel` (no behaviour change required) | Smaller units; mastery rules preserved (see §6) |
| W1.9 | P2 | Distinct Tools→SHAFT action icons + shorter labels (F12, F13, F21) | No identical icons; no truncated popup at default width |

### Phase 2 — Core human workflows

#### 2a — Web / general (P0 product)

| ID | Priority | Work item | Acceptance |
|---|---|---|---|
| W2.1 | P0 | New Project / Module wizard (`ModuleBuilder` / `DirectoryProjectGenerator`) | J2 AC met from Welcome screen |
| W2.2 | P0 | Run → evidence → Allure/Doctor loop (Coach) | J4 AC web path; Doctor not first wall |
| W2.3 | P0 | Record / Pick Locator without expert mode; insert-at-caret obvious | J3 AC |
| W2.4 | P0 | Properties profile card + schema-driven editor (field F2 core): autocomplete, deprecated flags, effective value+source, secret mask | Complements existing completion; secret-typed keys masked |
| W2.5 | P1 | Secrets guardrails (field F4): detect secrets files, gitignore ensure, mask console/Allure, `.example` generator | Pre-commit scan may be optional hook doc; no secrets in diffs from plugin actions |
| W2.6 | P1 | Assistant as optional layer after MCP verified; short slash list; plain-language agent failures; reattach UX | J5 AC |

#### 2b — Mobile / cloud track (field P0)

| ID | Priority | Work item | Acceptance |
|---|---|---|---|
| W2.7 | P0 | Target picker on gutter/run (field F1): Platform, Execution, Device+OS, App, retry default 0 | Invalid combos blocked; named Run Config; project files unchanged |
| W2.8 | P0 | Cloud session awareness (F5): running/queued vs license limit; stop orphan offer; link session video/logs | Warn before second session on 1-slot license |
| W2.9 | P1 | Cloud app upload + id registry (F3) — **spike first** (C8) | Registry `{platform,version,build,uploadedAt,id,alias}`; credentials from secrets only |
| W2.10 | P1 | Allure open / merge / clean + tags/groups when available (F7, #7549) | Merge Android+iOS selected runs; jump to source on failure |
| W2.11 | P1 | Locator inspector → page object (F8): hittable/visible/enabled; locator recommendation order | Cross-platform pair fields; flags non-hittable |
| W2.12 | P2 | Local Appium/emulator control (F6) | Cold → ready one click; port conflicts reported |

#### 2c — Platform UI tests

| ID | Priority | Work item | Acceptance |
|---|---|---|---|
| W2.13 | P1 | **IDE Starter + Driver** smoke (O2): Marketplace ZIP → cold IDEA → install → tool window → first-run happy path; Remote Robot fallback only if Starter cannot run headless in CI | Scheduled / `workflow_dispatch`; PR gate stays unit tests |
| W2.14 | P1 | Screenshot regen scoped to `ShaftPluginScreenshotRendererTest` only | No full-suite + screenshotDir latch regressions |

### Phase 3 — Reliability & Marketplace polish

| ID | Priority | Work item | Acceptance |
|---|---|---|---|
| W3.1 | P0 | Fix Phase 0 journey defects; EDT / flake regressions | Scorecard ≥ threshold agreed in W0.4 |
| W3.2 | P1 | Publish babysit + `assert_intellij_marketplace_receipt.py`; never retry verify around single-use version upload | Receipt = Gradle BUILD SUCCESSFUL; miss label path intact |
| W3.3 | P1 | Coverage toward #7080 without weakening tests | Hotspots shrink after presenter split |
| W3.4 | P1 | Marketplace gallery refresh from **signed** Carl flows; verified vendor if eligible; optional 30s GIF | Screenshots match J1–J4 |
| W3.5 | P2 | Re-enable searchable options when Windows hang gated/fixed | Settings search finds SHAFT keys |
| W3.6 | P2 | SHAFT version upgrade assistant (field F9) | Detect newer BOM, changelog behaviour diffs, bump + smoke + rollback offer |

### Phase 4 — Expert / power features (progressive disclosure only)

| ID | Priority | Work item | Acceptance |
|---|---|---|---|
| W4.1 | P2 | Expert MCP catalog / Design→Automation pack behind explicit Expert toggle | Hidden until journeys pass |
| W4.2 | P2 | Failure triage view noise collapse (field F11 / #6827 class) | First real error highlighted |
| W4.3 | P2 | API request builder → code (F10); snippets (F12); run guardrails (F13); tracker export (F14); skills pack single install (F15); bug-report helper (F16) | Each behind Expert or Setup cards; version-aware schemas |
| W4.4 | P2 | Flake / visual baseline analytics polish | No lead in IA |

---

## 5. Test and acceptance approach

| Layer | What | Gate |
|---|---|---|
| Unit / panel | Existing JUnit 5 + fakes; grow as megaclasses shrink | **PR gate** |
| Fixture | `BasePlatformTestCase` / light fixtures where Application required | PR gate where already used |
| Icon / theme contracts | No PNG-in-SVG; dual-leg colors; iconMapper present | PR gate |
| Mastery regression | MCP `isError` fail-closed; listener identity; Disposable-scoped MCP; setup badges from real checks; session reattach | PR gate + journey |
| Platform UI smoke | IDE Starter + Driver on Marketplace ZIP (W2.13); Remote Robot fallback only | Schedule / dispatch |
| Human acceptance | Owner or Coach walk J1–J5 (J6 when 2b lands) from Marketplace alone | **Release gate for revamp** |
| Screenshot | `ShaftPluginScreenshotRendererTest` only | Manual/CI scoped |
| Machines | ≥8 GiB free for Platform tests; shared box not evidence of green (C7) | Docs on epic |

Do not weaken existing tests to pass a rewrite.

---

## 6. Reliability rules (must survive rewrite)

From Chaos mastery input — bind as acceptance on Phase 1–3:

1. MCP `isError: true` inside OK JSON-RPC envelope = **failure** (PR #3396).
2. Capture `this::method` as a **field** before add/remove listener pairs (#3621).
3. Prefer `PlaceholderTextArea` / manual paint over `StatusText` for wrapping empty states.
4. Lifecycle-scope MCP pollers to tool-window `Disposable`.
5. Setup badges only from real verification (PR #3427).
6. Recordings owned by tool-window service, not ephemeral agent turns (PR #3431).
7. Assistant session death shows exit reason / reattach (#6748 class).
8. Keep `shaft-intellij/gradlew` wrapper coupling; monorepo `processResources` needs engine trees — no sparse-only `check` claim.

---

## 7. Risks

| Risk | Impact | Mitigation |
|---|---|---|
| Painting from unsigned / stale mocks | Wrong strip icons / IA | Use only SIGNED r2 pack @ `eef3c90`; ignore pre-SIGN assets |
| Megaclass rewrite churn | Regressions in Assistant | Presenter split first; behaviour locks via tests |
| Cloud upload (F3) needs new MCP surface | Scope creep | Spike W2.9; cut to registry-only if upload blocked |
| `require-restart=true` | Every shell change costs full IDE restart | Batch Phase 1 icon+color+DSL; release notes |
| Publish single-use version | Failed publish burns version | Keep verify≠upload; receipt script; babysit |
| Shared-box false greens | Lies in status | CI/dedicated only for Platform UI |
| Expert features leaking early | Cognitive load | Progressive disclosure gate on scorecard |
| Mobile invalid combos | Late cloud failures | Pre-launch validation in F1 |

---

## 8. Out of scope

- Rewriting `shaft-mcp` / Capture / Doctor engines unless a journey proves a protocol gap.
- Non-IntelliJ IDEs.
- Changing the SHAFT Java fluent API.
- Client names, prices, trainee data, or private attendance in UI/Marketplace/issues.
- Re-introducing pre-SIGN (REJECTED) mock pixels; only SIGNED r2 pack is binding.

---

## 9. Ownership and handoff

| Role | Owns |
|---|---|
| Chaos | Child issues under #7568, implementation PRs, CI/Platform robots, publish babysit |
| Carl | Design resubmit to SIGN; later Marketplace gallery assets after Phase 1 shell |
| Field validators | Real-user validation of J6 / mobile track; no client names in tickets |
| Coach | Journey scorecard acceptance; design SIGN/REJECT; this SPEC |

Suggested first Chaos slice after SPEC land: **W0.2** (IA delete/quarantine) + **W1.1–W1.3** (icons + colors) in parallel with **W0.3** journey recording on a capable machine.

---

## 10. Evidence index

- Coach audit `ee3af69` · Chaos `db4319e` · mobile/API input `442d471` · Carl audit `ebeefe0` · Carl pack r2 `eef3c90` (SIGN) · Coach design review @ `efbed24`
- Plugin module: `shaft-intellij/` @ engine `main` tip audited `6ffde6c` / pluginVersion `10.4.20261029`
- Mastery: `shaft-skills/ce-pack/references/shaft-mastery/intellij-plugin.md`
- Publish: `.github/workflows/publish-intellij-plugin.yml`, `mavenCentral_cd.yml` IntelliJ job, `scripts/ci/assert_intellij_marketplace_receipt.py`

---

## 11. Public provenance

- Consolidated from Coach SPEC commit `2e53581` (private coach workspace branch `coach/intellij-plugin-revamp-spec`).
- **O2 decision recorded here by Chaos** (IDE Starter + Driver; Remote Robot fallback).
- **O1** (exact tab labels) remains open pending journey scorecard.
- **O3 SIGNED** — Carl pack r2 @ `eef3c90`; Phase 1 visuals from mocks + DESIGN-SYSTEM + RUN-TAB-WIRE (UI needs only in public repo).
