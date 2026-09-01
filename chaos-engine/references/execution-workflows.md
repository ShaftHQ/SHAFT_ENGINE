# Execution workflows

This file is the sole normative owner of execution-workflow selection. Roles,
delegation, transports, and host adapters link here rather than redefining it.

## Select one workflow

| Workflow | Select when | Main orchestrator duties |
| --- | --- | --- |
| `SOLO` | Delegation is unsuitable, unavailable, or would cost more than the bounded work. | Plan, implement, verify, review, and complete the Learning Session. |
| `ORCHESTRATOR + SINGLE IMPLEMENTER` | One bounded implementation unit, or dependent units that must remain serial, has a qualified implementer. | Plan, specify, inspect, unblock, verify, integrate, review, and complete the Learning Session; do not implement. |
| `ORCHESTRATOR + PARALLEL IMPLEMENTERS` | Two to four independent, file-disjoint implementation units have qualified implementers. | Retain the single-implementer duties; assign isolated worktrees and integrate only after independent verification. |

Select from work shape and qualified capacity, never from a provider, model, or
marketing label. Default to `ORCHESTRATOR + SINGLE IMPLEMENTER` when delegation
adds value; select `ORCHESTRATOR + PARALLEL IMPLEMENTERS` only for independent
file scopes and cap writers at four. This is an explicit cap of four parallel
agents, not a transport-specific default. Reduce parallel work to one
implementer when capacity drops. Use
`SOLO` when no qualified implementer remains. Review is not an implementation
stream. Finish or hand over owned implementation before switching workflows.

## Transport is orthogonal

After workflow selection, choose the first available transport permitted by
the selected host and task boundary:

1. A qualified optional local OmniRoute process through the
   [OmniRoot skill](../skills/omniroot/SKILL.md).
2. A qualified host-native lower-capability implementer.
3. No qualified delegate: `SOLO`.

The transport does not change the selected workflow, role boundaries, tests,
review, learning, or completion duties. Canonical orchestration must probe the
fixed loopback endpoint before native fallback, with no endpoint prompt. If
`omniroute` is installed and `http://127.0.0.1:20128/api/health` fails, start
loopback only with `OMNIROUTE_SERVER_HOST=127.0.0.1 omniroute serve --port 20128 --no-open`.
Never install OmniRoute. Before every dispatch, query
`omniroute --output json models` and `omniroute --output json usage quota`
(no cache files). Rank remaining free candidates dynamically from live ids:
lowest applicable class first; architecture/review uses most-intelligent only.
Receipts and repository files never persist route, model, or provider IDs.
`RUNTIME_EXHAUSTED`, an empty remaining catalog, or sealed-launcher exit code
`78` falls back to the current host session's native models or `SOLO`. OmniRoute is absent,
unhealthy, unauthenticated, or unqualified does the same. This is
normal fallback, not harness failure.
