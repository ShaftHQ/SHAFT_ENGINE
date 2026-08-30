# Execution workflows

This file is the sole normative owner of execution-workflow selection. Roles,
delegation, transports, and host adapters link here rather than redefining it.

## Select one workflow

| Workflow | Select when | Main-agent duties |
| --- | --- | --- |
| `SOLO` | Delegation is unsuitable, unavailable, or would cost more than the bounded work. | Plan, implement, verify, review, and complete the Learning Session. |
| `ORCHESTRATOR + SINGLE IMPLEMENTER` | One bounded implementation unit, or dependent units that must remain serial, has a qualified implementer. | Plan, specify, inspect, unblock, verify, integrate, review, and complete the Learning Session; do not implement. |
| `ORCHESTRATOR + PARALLEL IMPLEMENTERS` | Two to four independent, file-disjoint implementation units have qualified implementers. | Retain the single-implementer duties; assign isolated worktrees and integrate only after independent verification. |

Select from work shape and qualified capacity, never from a provider, model, or
marketing label. Reduce parallel work to one implementer when capacity drops.
Use `SOLO` when no qualified implementer remains. When OmniRoute is absent, it
is a normal condition and never prevents any workflow from being selected.

## Transport is orthogonal

After workflow selection, choose the first available transport permitted by
the selected host and task boundary:

1. A qualified optional local OmniRoute process.
2. A host-native lower-capability implementer.
3. No delegate: select `SOLO`.

The transport does not change role boundaries, tests, review, learning, or
completion duties. OmniRoute must be detected but never installed, started,
authenticated, configured, or selected automatically. Its runner fails closed
unless loopback health, a current operator attestation, and endpoint-key
presence qualify it.
