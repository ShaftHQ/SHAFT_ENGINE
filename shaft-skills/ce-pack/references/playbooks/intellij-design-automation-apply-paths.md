# Design / Automation MCP apply paths (#6012)

Every Design/Automation tool that paints IDE state needs:

1. A dedicated `apply*Json` (or typed apply) on the panel/model.
2. A router discriminator on a stable field (or explicit tool name) in
   `DesignStagePanel.applyToolResult` — never reuse `applyAnalysisJson` for a
   different JSON shape.
3. A panel unit test that applies a fixture and asserts the UI model updates.

Current discriminators include `shaftType`/`shaftMethod` → gap map,
`handoffAllowed` → readiness, `automationPrefill` → handoff.

See also [intellij-ui-test-conventions.md](intellij-ui-test-conventions.md) for accessibleName
(#6013) and Live record persistence (#6014).
