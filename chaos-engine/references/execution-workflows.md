# Execution workflows

This file is the sole normative owner of execution-workflow selection. Roles,
delegation, transports, and host adapters link here rather than redefining it.

## Select one workflow

| Workflow | Select when | Main orchestrator duties |
| --- | --- | --- |
| `SOLO` | Delegation is unsuitable, unavailable, or would cost more than the bounded work. | Plan, implement, verify, review, and complete the Learning Session. |
| `SINGLE ORCHESTRATOR` (orchestrator + single implementer) | One bounded implementation unit, or dependent units that must remain serial, has a qualified implementer. | Plan, specify, inspect, unblock, verify, integrate, review, and complete the Learning Session; do not implement. |
| `PARALLEL ORCHESTRATOR` (orchestrator + parallel implementers) | Two to four independent, file-disjoint implementation units have qualified implementers. | Retain the single-implementer duties; assign isolated worktrees and integrate only after independent verification. |

Select from work shape and qualified capacity, never from a provider, model, or
marketing label. Default to `SINGLE ORCHESTRATOR` when delegation adds value;
select `PARALLEL ORCHESTRATOR` only for independent file scopes and cap writers
at four. Reduce parallel work to one implementer when capacity drops. Use
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
review, learning, or completion duties. OmniRoute must be detected but never
installed, started, authenticated, configured, or selected automatically. Its
runner fails closed unless loopback health, a current operator attestation, and
endpoint-key presence qualify it. When OmniRoute is absent, unhealthy, or
unqualified, continue with a qualified host-native implementer or `SOLO`; this
is a normal fallback, not a harness failure.
