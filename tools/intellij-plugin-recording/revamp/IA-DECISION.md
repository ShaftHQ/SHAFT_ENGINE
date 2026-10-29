# IntelliJ plugin IA decision (W0.2 / #7568)

**Decision (2026-10-29):** Agent-first product IA. Orphaned three-stage panels deleted.

| Human tab (SIGNED Carl r2 default) | Product surface | Must not restore |
|---|---|---|
| Assistant | Agent chat | — |
| Tests | Workflow + execution log | — |
| Setup | First-run + MCP verify | — |
| ~~Design / Automation / Reporting~~ | Deleted `*StagePanel` (~1.2k LOC) | Never reintroduce as primary IA |

Live selector labels remain `Agent` / `Workflow` / `Execution log` until journey scorecard (O1) renames them; mapping is 1:1.

Persistence key `Live record` stays on `ShaftToolWindowPanel.LIVE_RECORD_TAB` for #6014 Guided→Live record canonicalize.

Refs: epic #7568, child W0.2, SPEC `tools/intellij-plugin-recording/revamp/SPEC.md` C1.
