# Delivery workflow phase gates

Maps the ChaosEngine delivery loop to **hooks** (machine-checkable) vs **skills**
(judgment).

| Phase | Guarantee owner | Hook / surface | Skill judgment |
| --- | --- | --- | --- |
| Triage / consult | Skill | — | [consult-first](consult-first.md), router skill |
| Research receipt | Hook when enforced | PreToolUse research-before-mutation gate | [research-receipt](research-receipt.md) |
| Implement (Do) | Hook | PreToolUse session identity, catastrophic deny, worktree | playbooks / roles |
| Check | Skill + tests | PostToolUse records outcomes | consolidated proof commands |
| Optional adversarial | Skill | — | independent PR review |
| Learning Session | Hook | Stop requires terminal Learning Session | [learning](../learning.py) + [self-improve](../skills/self-improve/SKILL.md) |

## Research-before-mutation (enforced path)

By default, research-before-mutation remains skill guidance so mechanical
one-file work stays unblocked.

Owners who want a hard guarantee set:

```text
export CHAOS_ENGINE_ENFORCE_RESEARCH_RECEIPT=1
```

When set, `hooks/guard.py` denies **PreToolUse** stateful mutations until the
session ledger records a `research-preflight` entry. Record one with the
zero-LLM CLI:

```text
python3 .chaos-engine/hooks/reflection.py research-preflight --session-id "$SESSION"
```

Hosts that honor exit-2 / native deny (`HostCapability.process_exit2_honored`)
hard-block; others still emit deny payloads (see [host-parity-matrix](host-parity-matrix.md)
GAP-EXIT2).

## Delivery is merge

Implemented repository changes are not complete when the files exist only in
the working tree or on a local branch. By default, finish the delivery:

1. Commit on a `ChaosEngine/…` branch taken from the fetched default branch.
2. Push that branch.
3. Open a pull request. This repository allows merge commits only.
4. Watch required checks and merge the pull request. A green local test is not
   a merge.

Stop short of that sequence only when the owner explicitly asked for a local
change, a draft, or a plan, or when a hard external blocker (credentials,
denied permission, or a required check that cannot be fixed in this session)
is stated with evidence.

## Invariants

- Skills still own thoroughness of the eight-step receipt content.
- Hooks only prove that a preflight marker exists before mutation when enforced.
- Safety denials and session-identity rules remain higher priority than this gate.
- Learning Session Stop gate fires after confirmed delivery only when a trigger
  fired (failure, surprise, owner ask). Harness lessons, findings, and potential
  enhancements are GitHub issues only. Do not write them to a local queue or
  into chat. Product lessons may report `product queued N` or `nothing durable`.
- Harness parity: lasting policy lives in the portable overlay, not one agent.


## Triage-scaled research gate

The zero-LLM phase ledger (`.chaos-engine-state/phase-ledger.json`) records
triage and phase markers without an LLM:

```text
python3 .chaos-engine/phase_ledger.py record --session-id "$SESSION" --phase triage --triage public-contract
python3 .chaos-engine/phase_ledger.py show --session-id "$SESSION"
python3 .chaos-engine/phase_ledger.py summary
```

| Triage | Research-before-mutation |
| --- | --- |
| `public-contract` / hard-to-reverse | **Hard** — auto-enforced (same deny as env flag) |
| `one-file` (mechanical) | **Soft** — never blocks from triage; also softens env flag |
| `one-module` / unset | Env flag `CHAOS_ENGINE_ENFORCE_RESEARCH_RECEIPT=1` only |

`doctor --json` includes a bounded `phaseLedger` summary (no secrets).
