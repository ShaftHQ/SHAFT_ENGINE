# MCP Protocol Engineering (shaft-mcp)

## Transport truths
stdio transport = newline-delimited JSON-RPC on stdin/stdout; stdout is
PROTOCOL-ONLY — any stray print (logging, Maven banners, JVM warnings)
corrupts the stream. All diagnostics go to stderr. The proven E2E validation
pattern: a throwaway Python stdio driver against the real
`java @shaft-mcp.args` process with cwd = a consumer project (repo workflow
`live-validating-shaft-mcp-fixes-end-to-end-via-a-scripted-stdio-driver`).

## Error semantics (three layers, all different)
1. Transport/JSON-RPC error — request failed.
2. `isError: true` in a SUCCESSFUL tool result — the tool failed; clients
   that treat the envelope's success as the verdict mask every real failure.
3. Domain "soft fail" in payload text — needs explicit checks.
Always propagate layer-2 text to the user/client verbatim.

## Workspace roots
The workspace root resolves from the CLIENT LAUNCH DIRECTORY; installers
write only `-Dshaft.mcp.fallbackWorkspaceRoot`, never `-Duser.dir` or a hard
workspace root, and a bare user home must never become a workspace
(PR #3428 decisions, validator-enforced). Scoping to `Path.of(".")` (IDE
cwd) instead of the open project is a recurring bug shape.

## Long-lived state vs one-shot turns
Recordings/captures are stateful sessions that must outlive the initiating
request. Failure shapes seen live: capture session JSON vanishing mid-run on
Windows (non-atomic replace-move under AV/file-watcher contention) wedging
status into an "already active vs not started" split-brain — lifecycle code
must self-heal from a missing store, never wedge until process death
(PR #3431).

## Client-side gotchas
- `claude --print` + `acceptEdits` auto-DENIES every Bash/MCP tool call
  unless `--permission-prompt-tool` (an MCP-over-HTTP approval bridge) is
  wired — flag exists in the binary but not `--help` (PR #3420/#3411).
- Live-probe any CLI's wire behavior with a script before implementing
  against it; a prior SHAFT feature shipped dead because it assumed a
  control-channel that doesn't exist on raw stdio (PR #3373→#3410).

## Tool design
Verb_noun names, flat args, one tool = one user-meaningful outcome,
structured content out. Reject dangerous generated code server-side
(`test_code_guardrails_check` pattern) rather than trusting client prompts.
