# Grok host parity and CLI-over-MCP (#5780–#5785)

- Overlay-only skills. Never vendor Grok pdf/pptx/imagine/game-* bundles into ChaosEngine.
- Prefer `gh` when healthy; doctor/repair strips user-host GitHub MCP ids. When gh is missing/unauthenticated, leave GitHub MCP. Project overlay never publishes GitHub MCP.
- Git-tracked `.mcp.json` must not embed workstation-absolute maven-tools-mcp java/jar paths; use `.chaos-engine/tool.py maven-tools-mcp` shared-cache launcher or docker image mode.
- Enable plugins: chaos-engine, caveman, ponytail only. Doctor heals/fails extras. Do not rewrite marketplace.json without overlay content change.
- `learn_traces.py collect` parses Grok/Claude/Codex/(Gemini) session trees into redacted session JSON + manifest. Map-reduce stays with isolated host agents.
- GAP-GROK-BUNDLED documents the unfixable Grok product-bundle limit.
