# SHAFT CLI

Use when a SHAFT task needs a deterministic one-shot command, machine-readable output, offline discovery, code generation from a persisted recording, or a grounded lookup with `shaft-cli guide search`. `shaft` is an accepted alias of the `shaft-cli` launcher.

Emulator cold-boot, memory pressure, and the one-device Appium rule: [mobile session policy](../mobile-session-policy.md).

Command forms: [CLI cache](../cli-cache.md). Names and arguments: [tool catalog](../shaft-mcp-tools.md).

Use SHAFT CLI when a deterministic command, exit code, JSON result, or shell
pipeline is the deliverable. Exact command forms live in the [command cache](../cli-cache.md).
Tool names and essential arguments live in the [tool catalog](../shaft-mcp-tools.md).

Assistants without MCP access (shell only) use the same tools through the
installer's stable `~/.local/bin/shaft-cli` shim: `shaft-cli tools --cached`, then
`shaft-cli call <tool> key=value`. The installer prints a paste-ready stdio
definition for MCP hosts it did not configure.

## Ten practices

1. Prefer CLI for stateless, repeatable, scriptable work; prefer direct MCP for interactive work or state that already lives in the connected client. [`SHAFT-MCP`]
2. Use curated aliases for common intent and `shaft-cli call <tool>` for parity with every exact tool name in the [tool catalog](../shaft-mcp-tools.md). [`SHAFT-MCP`]
3. Treat generic `call` as name dispatch only: it does not infer that browser, element, driver, mobile, or capture tools need persistent state. Start a session first for any such sequence. [`SHAFT-MCP`]
4. Run `shaft-cli session start`, confirm `session status`, execute every related stateful command, then `session stop`; one-shot child processes cannot share state. [`SHAFT-MCP`]
5. On curated stateful aliases, use `--stdio-ok` only for a deliberately disposable call; generic `call` already falls back to one-shot and does not use this flag as a state-safety gate. [`SHAFT-MCP`]
6. Supply simple inputs as `key=value`; use `--args` for nested JSON, remembering that later `key=value` inputs override matching JSON keys. [`SHAFT-MCP`]
7. Request `--json` when another program will consume the result, and gate on both exit code and returned error status. [`SHAFT-MCP`, `ALLURE-RESULTS`]
8. Use the checked-in catalogs first; use `shaft-cli tools --cached` for offline discovery and live `shaft-cli tools` only when runtime drift must be inspected. [`SHAFT-MCP`]
9. Use `shaft-cli codegen --session <recording>` for deterministic codegen from a persisted recording; specify backend and output flags when defaults are ambiguous. [`SHAFT-MCP`, `SHAFT-GUIDE`]
10. Quote JSON and shell metacharacters for the active shell, keep secrets out of arguments and logs, and preserve generated evidence paths for review. [`SHAFT-REPORTING`, `ISTQB-CTFL`]

## Valid examples

- Run `shaft-cli guide search query="browser assertions" maxResults=3 --json` in a script and fail the step on a nonzero exit code.
- Run `shaft-cli call test_code_guardrails_check --args '{"language":"java","code":"..."}' --json` for a stateless machine-readable gate.
- Start a persistent session before `shaft-cli call driver_initialize engine=web`, `shaft-cli call browser_navigate targetUrl=https://example.test`, and `shaft-cli call driver_quit`.
- Generate code with `shaft-cli codegen --session recordings/checkout.json --backend webdriver` without starting MCP state.
- `shaft-cli setup start` on Linux starts the managed Android emulator. Cold-boot, the memory cap, and the one-device Appium rule are in [the mobile session policy](../mobile-session-policy.md).

## Boundary

- If the task is exploratory, already owns an MCP connection, needs direct schema-guided calls, or must keep live state without managing a CLI daemon, route to [MCP](mcp.md).
- Three-stage CLI families (#5943): `shaft-cli design` (Design), `shaft-cli capture` + offline `shaft-cli codegen` (Automation), `shaft-cli report` + `shaft-cli doctor` (Reporting). Healing belongs to Reporting (Stage 3), not record/codegen.

## CLI usage (#6909)

- Prefer `shaft-cli call` for a stateless tool so the MCP schema is not loaded.
- Ask `shaft-cli tools --cached` before a live tool listing, and do not paste the live listing into the chat.
- Generated mobile Java taps with `driver.touch().tap(locator)` and never `tapByCoordinates`.
- Prefer the checked-in catalog over dumping `shaft-cli tools` live output into the chat.
- Inside a `shaft-cli session`, mobile taps still use a locator, not viewport coordinates.

## Guide search

## Ten practices

1. Search the live official guide before writing unfamiliar or version-sensitive SHAFT syntax, configuration, migration, or troubleshooting advice. [`SHAFT-GUIDE`]
2. Use exact `shaft_guide_search` through direct MCP when already connected, or `shaft-cli guide search` for a stateless scriptable lookup. [`SHAFT-MCP`]
3. Phrase one narrow query with the surface, action, and constraint, such as `Playwright browser title assertion`; avoid broad product questions. [`SHAFT-GUIDE`]
4. Request only a few high-value matches first, then refine with returned terminology instead of loading the whole guide. [`SHAFT-MCP`]
5. Read the title, section, excerpt, code blocks, guidance rules, warnings, and source URL together; do not treat one snippet as the full contract. [`SHAFT-GUIDE`]
6. Cite the returned official URL beside the claim or code it supports, preserving enough section context for review. [`SHAFT-GUIDE`]
7. Cross-check live guide syntax against the user's installed SHAFT version and current repository exemplars before changing code. [`SHAFT-GUIDE`, `ISTQB-CTFL`]
8. If results are empty or conflicting, reformulate once with official class or method names; report the gap rather than inventing an API. [`SHAFT-GUIDE`]
9. Prefer the current official guide over blogs, generated chat answers, stale examples, or memory; use external sources only for the non-SHAFT discipline they own. [`SHAFT-GUIDE`, `SELENIUM-PRACTICES`]
10. Route the grounded result to the correct specialist and verify compiled behavior; search evidence alone does not prove the user's flow. [`ISTQB-CTFL`]

## Valid examples

- Search `SHAFT Locator Builder ARIA role` before creating a generated web locator, then cite the returned locator-builder page.
- Search `SHAFT.API target status code response assertion` before implementing a service check.
- Search `mobile touch swipe element into view` before composing a mobile gesture chain.
- Search `browser verifyThat versus assertThat` before choosing soft or hard validation semantics.

## Boundary

- Return authoritative guide evidence and current syntax, not an unverified implementation; route code, actions, locators, or assertions to their focused specialist.
