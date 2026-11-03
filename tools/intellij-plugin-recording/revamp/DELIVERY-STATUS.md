# IntelliJ revamp delivery status (#7568)

Updated: 2026-11-03 (Africa/Cairo)

## Closed by this delivery PR

### Already on main from #7621 (never auto-closed — Fixes keywords were backtick-wrapped)

| ID | Ticket |
|---|---|
| W0.1 | #7572 |
| W0.2 | #7573 |
| W0.4 | #7575 |
| W0.5 | #7576 |
| W1.1 | #7577 |
| W1.2 | #7578 |

### Implemented in this PR

| ID | Ticket | Evidence |
|---|---|---|
| W1.3 | #7579 | `ShaftUiColors`, no-emoji `ShaftStatusPresentation`, dual-leg `MessageStyleRegistry` |
| W1.4 | #7580 | `ShaftUiMetrics` JBFont/spacing tokens |
| W1.5 | #7581 | Kotlin UI DSL `ShaftSettingsGroupsForm` chrome; id `shaft.settings` unchanged |
| W1.6 | #7582 | `ShaftRunOverridesLabels.PROJECT_DEFAULT` on overrides panel |
| W1.7 | #7583 | `shouldBeAvailable`, Open action visibility, Tests empty-state CTA |
| W1.8 | #7584 | `ShaftAssistantPresenter` / `ShaftMcpSetupPresenter` facades |
| W1.9 | #7585 | Distinct Tools menu icons/labels |
| W2.1 | #7609 | `ShaftMavenModuleBuilder` + scaffold |
| W2.2 | #7586 | `AllureReportActions` evidence-first helper (Doctor remains optional) |
| W2.4 | #7588 | `ShaftPropertiesProfileCard` |
| W2.5 | #7589 | `ShaftSecretsGuardrails` |
| W2.14 | #7598 | `ScreenshotRegenScopeTest` |

### Already satisfied on main; close with this PR

| ID | Ticket | Evidence |
|---|---|---|
| W2.3 | #7587 | `RecordShaftFlowHereAction` non-expert since #5942 |
| W3.2 | #7600 | `scripts/ci/assert_intellij_marketplace_receipt.py` + tests on main |

## Still open (honest blockers)

| ID | Ticket | Why |
|---|---|---|
| W0.3 | #7574 | Human J1–J5 scorecard needs ≥8 GiB + Marketplace ZIP |
| W2.6 | #7590 | Full “Assistant optional after MCP verified” UX not completed (only plain-language failure helper) |
| W2.7–W2.12 | #7591–#7596 | Mobile/cloud track |
| W2.13 | #7597 | IDE Starter Marketplace ZIP smoke |
| W3.1 | #7599 | Depends on W0.3 defects |
| W3.3–W3.6 | #7601–#7604 | Coverage / gallery / searchable options / upgrade assistant |
| W4.* | #7605–#7608 | Expert features gated on journey scorecard (SPEC C6) |

Epic #7568 stays open until human journey gate passes.
