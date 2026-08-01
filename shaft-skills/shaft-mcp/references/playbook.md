# MCP playbook

Use direct SHAFT MCP calls when the client must retain live tool state or when
structured schemas and interactive results are more valuable than a shell
transcript. Exact names live in the [generated MCP catalog](../../references/shaft-mcp-tools.md).

## Ten practices

1. Choose MCP for interactive exploration and stateful browser, driver, mobile, element, or capture sequences; keep one connected client for the whole sequence. [`SHAFT-MCP`]
2. Choose `shaft-cli` instead for repeatable stateless automation, offline discovery, persisted-recording codegen, or shell pipelines. [`SHAFT-MCP`]
3. Resolve the exact tool name from the generated catalog or live tool index; never invent, pluralize, or revive a retired name. [`SHAFT-MCP`]
4. Call the named MCP tool directly when the client exposes it; use a generic bridge only when direct tools are unavailable, and pass the same exact name and schema-validated arguments. [`SHAFT-MCP`]
5. Load only the required schemas, batching schema discovery when the client supports it; never copy schemas into skills or prompts. [`SHAFT-MCP`]
6. Initialize the intended engine with `driver_initialize` before engine-dependent work, and close it with `driver_quit` when ownership ends. [`SHAFT-MCP`, `APPIUM`]
7. For mobile recording, initialize `mobile_native` or `mobile_web` before `capture_start`; without an active engine, capture starts a managed desktop web recording. [`SHAFT-MCP`, `APPIUM`]
8. Treat screenshots, DOM, ARIA, accessibility trees, traces, and reports as bounded evidence; redact credentials and avoid returning raw sensitive state. [`SHAFT-REPORTING`, `W3C-ACT`]
9. Read structured warnings, readiness, `isError`, and returned next-tool guidance before the next call; a transport success is not a test verdict. [`SHAFT-MCP`, `ALLURE-RESULTS`]
10. Preserve authorization boundaries: obtain approval before visible browsers, devices, Inspector, replay, or other side effects, and never infer deploy or publish authority. [`SHAFT-MCP`, `ISTQB-TM`]

## Valid examples

- Inspect a checkout page with `browser_open_intent`, act with `element_click`, and confirm state with `browser_get_current_url` on one live MCP connection.
- Initialize `mobile_native`, inspect `mobile_get_accessibility_tree`, use `mobile_swipe`, then end with `driver_quit`.
- Record an approved flow through `capture_start`, `capture_checkpoint`, `capture_status`, and `capture_stop` without losing recorder state.
- Call stateless `shaft_guide_search` directly when the client already exposes its schema and the answer will guide an interactive task.

## Boundary

- For CI scripts, repeatable one-shot commands, offline tool discovery, or persisted-recording code generation, route to `shaft-cli`; MCP is not required merely because CLI delegates to the same tools.

Sources: [shared authoritative bibliography](../../shaft-developer/references/sources.md).
