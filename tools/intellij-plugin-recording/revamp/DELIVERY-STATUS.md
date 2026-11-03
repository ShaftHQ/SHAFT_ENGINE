# IntelliJ revamp delivery status (#7568)

Updated: 2026-11-03 (Africa/Cairo) — close-out PR after #7754

## Closed earlier

- #7754 closed W0/W1/W2 shell tickets: 7572–7589, 7598, 7600, 7609 (20 issues).

## Implemented in this close-out PR

| ID | Ticket | Evidence |
|---|---|---|
| W0.3 | #7574 | Marketplace ZIP → cold IDEA IC 2024.3.7.1 under xvfb; `evidence/closeout-20261103/` |
| W2.6 | #7590 | `ShaftAssistantPresenter` MCP gate + reattach / optional Assistant |
| W2.7 | #7591 | `ShaftTargetSelection` + overrides panel Platform×Execution×Device×App |
| W2.8 | #7592 | `ShaftCloudSessionAwareness` |
| W2.10 | #7594 | `AllureReportActions` merge / clean / listTagsAndGroups |
| W2.11 | #7595 | `LocatorPageObjectGenerator` |
| W2.13 | #7597 | `tools/intellij-e2e/smoke_marketplace_zip.sh` + e2e evidence |
| W3.1 | #7599 | Scorecard filled from e2e; EDT-safe helpers covered by unit tests |
| W3.3 | #7601 | New unit tests for close-out surfaces (toward #7080) |
| W3.4 | #7602 | Marketplace description already refreshed on main; evidence receipt retained |
| W3.5 | #7603 | `buildSearchableOptions` enabled on Linux/macOS (still off on Windows) |
| W3.6 | #7604 | `ShaftBomUpgradeAssistant` |
| W4.2 | #7606 | `FailureTriageNoiseCollapse` |

## Won't-do in this revamp (closed not-planned)

| ID | Ticket | Reason |
|---|---|---|
| W2.9 | #7593 | Cloud app upload + id registry is a multi-sprint spike; not needed for J1–J5 trust |
| W2.12 | #7596 | Local Appium/emulator control is out of scope for a companion plugin close-out |
| W4.1 | #7605 | Expert MCP catalog / Design→Automation waits on progressive disclosure after journeys |
| W4.3 | #7607 | API builder + snippets is a separate product surface |
| W4.4 | #7608 | Flake / visual baseline analytics polish deferred post-revamp |

Epic #7568 closes with this PR once the Fix list above merges.
