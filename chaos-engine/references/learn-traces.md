---
name: learn-traces
description: Use when session traces must be mapped, reduced, and verified into lessons without a host TUI runner.
---

# Learn traces

Portable map-reduce-verify contract for session traces. Host TUI workflow
runners may implement this; other hosts use isolated subagents and files. Do
not reconstruct a vendor workflow from memory. Do not copy host-only script
or pager mechanics here. Do not vendor a Grok TUI learn-traces workflow script (or equivalent
host pager/runner) into this tree.

## Phases

1. **Collect** — keep sessions where a human sat; drop synthetic, subagent,
   and (unless asked) headless runs. Redact credentials. Write a manifest with
   counts and drop reasons.
2. **Map** — one agent per batch of sessions. Extract repeated phrases, stale
   skill lines, unused loaded surfaces, and gaps. Cite session ids.
3. **Reduce** — fold map notes until one synthesis remains. Open candidate
   skill files as context; invent nothing.
4. **Verify** — three independent skeptics: phrases, stale lines, deletes.
   Keep or drop; add nothing. Fail closed on missing evidence.
5. **Report** — overview, action tables, coverage line, and a machine-readable
   action list. Git-tracked edits become patches or a PR, not home-dir copies.

A first-ever run is step/curate, never auto-delete. See
[harness-learn](harness-learn.md).

## Portable /learn (every host)

Installed path only (`.chaos-engine`). Never treat source `chaos-engine/` as the
runtime install.

Runner modules: [`learn_traces.py`](../learn_traces.py) (CLI + collect) and
[`learn_traces_mrv.py`](../learn_traces_mrv.py) (map-reduce-verify file contract).

One-command equivalent of host `/learn` for **git-tracked overlay** policy:

```bash
python3 .chaos-engine/learn_traces.py learn --out <run-dir>
```

Default `--offline` path fills the map/reduce/verify **file contract**
deterministically (scripts/CI). For live host Task/subagents:

```bash
python3 .chaos-engine/learn_traces.py learn --out <run-dir> --host-agents
# …isolated agents write map/batch-*.json, reduce/synthesis.json, verify/*.json…
python3 .chaos-engine/learn_traces.py finalize --run-dir <run-dir>
```

Stepwise:

| Step | Command |
| --- | --- |
| Collect | `python3 .chaos-engine/learn_traces.py collect --out <run-dir>` |
| Layout + prompts | `python3 .chaos-engine/learn_traces.py prepare --run-dir <run-dir>` |
| Offline map-reduce-verify | `python3 .chaos-engine/learn_traces.py offline --run-dir <run-dir>` |
| Report + actions | `python3 .chaos-engine/learn_traces.py finalize --run-dir <run-dir>` |

Collect parses Grok/Claude/Codex (and Gemini when present) session trees into
redacted `sessions/*.json` plus `manifest.json` with kept and drop counts. No
credentials in output.

`finalize` writes `report.md` (coverage line matches manifest kept count) and
`actions.json`. Action targets are git-tracked `chaos-engine/` /
`.chaos-engine/` PR paths or `patches/*.diff` — **never** `~/.grok/skills/...`
as the primary write.

## Thin host adapters

Same documented command on every supported host. Adapters are pointers only;
policy lives here + the runner under `.chaos-engine`.

| Host | How to invoke |
| --- | --- |
| Grok | Prefer CE `learn_traces.py learn` for overlay PRs. Grok TUI `/learn` may still curate `GROK_HOME` skills; it is not the owner of git-tracked ChaosEngine policy. |
| Claude | Router → Learn traces, or the CLI above. No TUI workflow file required. |
| Codex | Same CLI / router row. |
| Copilot | Same CLI / router row. |
| Gemini | Same CLI when session trees exist. |

Do not copy a host TUI workflow into this tree. Do not write learned skills under
`~/.grok/skills`. Do not require `~/.grok/bundled/skills/learn`.
