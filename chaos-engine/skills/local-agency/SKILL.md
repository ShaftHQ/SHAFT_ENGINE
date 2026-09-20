---
name: local-agency
description: >-
  Use when the adopter asks to delegate to local agents (OpenCode / OSS agency)
  against a READY local runtime instead of orchestrator-session subagents.
license: MIT
---

# Local agency (OpenCode)

Optional **local coding agency** path. Not a workflow owner; select the
canonical workflow in [execution workflows](../../references/execution-workflows.md)
first.

**Local agency vs session agents**

| Path | When | Owner |
| --- | --- | --- |
| Session subagents / Task | Default orchestrator labor on the host | Host adapter + [delegation](../../references/delegation.md) |
| Local agency (this skill) | Adopter asked for OpenCode / local OSS agency against local weights | This skill + READY runtime |

## Hard rails (never regress)

- Prefer a **READY** local runtime: FreeToken (`:1919`), then Ollama / LM Studio /
  llamacpp via [local-openai-compat](../local-openai-compat/SKILL.md), then Colibri (`:8000`)
  via [colibri](../colibri/SKILL.md) (frontier MoE; prefer FreeToken for coding agency).
- Configure OpenCode with **ephemeral** `OPENCODE_CONFIG` /
  `OPENCODE_CONFIG_CONTENT` that sets `enabled_providers` to the READY local
  provider. OpenCode merges global config; the allowlist keeps other providers
  out of that process. `--pure` only disables external plugins.
- **Never** write into durable `~/.config/opencode` (helper refuses those paths),
  **never** `ft launch` / `ft serve`, **never** start local servers from CE.
- Do **not** silently fall back to cloud OmniRoute. Missing local runtime:
  tell the operator; continue with session agents / `SOLO` / OmniRoute only when
  the adopter explicitly asked for that path.
- Do **not** install OpenCode from this skill. Operator install stays on vendor
  docs (README third-party table).
- Installer / doctor / status must **not** fail because OpenCode or a local
  runtime is missing.
- Never bind or probe a non-loopback URL from ChaosEngine.
- **ROG FreeToken bind (#6021):** Task/box writers must **not** claim FreeToken.
  `dispatch.py resolve --prefer freetoken` and
  [`require_rog_freetoken.py`](scripts/require_rog_freetoken.py) fail closed unless
  the hostname looks like ROG, cwd is the operator ROG checkout (see
  [local-agency guide](../../guides/local-agency.md)), or
  `CE_ALLOW_BOX_LOCAL_AGENCY=1`. Process-owner Shell with `machineId` on ROG is **mandatory** for ROG
  FreeToken/OpenCode writers (#6051). Do **not** dispatch those writers via
  Task until Grok Bot exposes `machineId` to Task/executor Shell. Diagnostic:
  [`assert_parent_rog_shell.py`](scripts/assert_parent_rog_shell.py). Clear error when FreeToken
  is not READY on this host.

## Probe → resolve → ephemeral OpenCode

Helper:
[`chaos-engine/skills/local-agency/scripts/dispatch.py`](scripts/dispatch.py). ROG gate: [`require_rog_freetoken.py`](scripts/require_rog_freetoken.py).

```text
python3 chaos-engine/skills/local-agency/scripts/dispatch.py resolve
python3 chaos-engine/skills/local-agency/scripts/dispatch.py --prefer freetoken config
python3 chaos-engine/skills/local-agency/scripts/dispatch.py argv --prompt '…' --workdir '<worktree>'
```

`resolve` ranks FreeToken, then OpenAI-compat peers, then Colibri. Use `--prefer colibri` when the adopter asked for Colibri. On `READY`, `config` /
`argv` emit ephemeral OpenCode material (`OPENCODE_CONFIG` path). Run OpenCode
yourself with that env; do not persist the config into the durable user file.
`argv` defaults include `--pure` and `--variant` (`medium`; use `low` or
`medium` for tool loops). `--no-pure` exists for callers that already isolate
plugins. `--pure` still only disables plugins.

## CE brief (design turns)

For design/spec-shaped local turns, build a locator-only system brief with [`ce_brief.py`](../../ce_brief.py) or via dispatch:

```text
python3 chaos-engine/skills/local-agency/scripts/dispatch.py brief --json
python3 chaos-engine/skills/local-agency/scripts/dispatch.py --prefer llamacpp config --with-ce-brief
python3 chaos-engine/skills/local-agency/scripts/dispatch.py --prefer llamacpp chat --prompt '…' --with-ce-brief
```

Do not dump full SKILL bodies into the model context (#6067/#6068).
Unit/eval contracts: [`ce-brief-unit-fixtures.json`](../../evals/ce-brief-unit-fixtures.json) (#6072).


## CE project pointers (OpenCode preflight)

Before emitting `config` / `argv` OpenCode material, `dispatch.py` fail-closes unless the
target worktree has ChaosEngine project pointers:

- `AGENTS.md`
- `.agents/skills/chaos-engine/` (install-generated skill adapter)

`--pure` only disables external plugins; it does **not** load ChaosEngine and does **not**
replace those pointers (#6070). Missing pointers → `state=UNHEALTHY` with `ce_pointers.missing`.

## Mechanical dispatch (small-context local models)

Treat a READY local coder as a **mechanical runner**, not a designer.
Never use it for independent adversarial review. Review stays a new instance of
the host session model (see [delegation](../../references/delegation.md)).

1. Orchestrator writes one bounded command (a short script with exact paths).
   The OpenCode prompt is **only** that executable line (e.g.
   `bash /abs/path/apply.sh`). No extra English nouns such as
   `git add product files` — those trigger Glob/Grep then
   `context_length_exceeded` (#5996/#5997). Keep `git add` of exact paths
   **inside** the script; commit stays orchestrator-owned.
2. OpenCode invokes **exactly that one bash command**. Multi-step specs in
   chat are writer failures: EXIT 0 with zero tool calls is writer failure,
   not success. Glob/Grep of prompt English before the named bash command,
   or overflow with no worktree mutation, is writer failure — run the same
   command in the worktree; do not retry the oversized prompt.
3. OpenCode bash tool wall-clock is **120s** (#5998). Maven/Gradle apply
   scripts that need longer must not be left half-applied: treat the 120s
   kill as writer failure and re-run the **same** apply.sh from the
   orchestrator (scripts must be idempotent). Prefer patch-only OpenCode
   when tests exceed two minutes.
4. Never glob gitignored trees (including Memory). Use pathlib / exact paths.
   Zero glob matches on a gitignored path is not success.
5. Keep tool output tiny. Do not feed verbose unit-test logs into the next
   model turn.
6. On `context_length_exceeded`, fail the turn. Shrink prompt and tool output
   and drop a high/reasoning variant before asking the operator to serve a
   larger checkpoint. Advertised `context_length` from `/v1/models` is not
   usable KV. Do not OpenCode-`Read` a 200-line Java file; pass the exact
   line change. Soak on this class of host: keep the prompt well under 8k
   combined prompt+generation.
7. Size-class soak: prove the current READY checkpoint with these knobs.
   Only if quality is still insufficient **and**
   [`probe_hardware.py`](../local-coding-delegate/scripts/probe_hardware.py)
   returns `medium` or `large` may the operator serve the next known-good
   coding MoE from **vendor docs**. `refuse`: do not recommend a larger
   checkpoint. ChaosEngine never downloads weights or starts serve.

Example (session only; model id is not a git pin):

```text
python3 chaos-engine/skills/local-agency/scripts/dispatch.py --prefer freetoken config
# export OPENCODE_CONFIG from the JSON env map for one process only
OPENCODE_CONFIG=<ephemeral-path> opencode run --pure --variant medium --dir '<worktree>' '<one command>'
```

Hardware size-class hint:
[`local-coding-delegate/scripts/probe_hardware.py`](../local-coding-delegate/scripts/probe_hardware.py)
(`small` / `medium` / `large` / `refuse`).


## Session token usage (#6069)

When `dispatch.py chat` returns OpenAI-compat `usage` and `--session-id` is set, dispatch records
`session_token_usage.py` with `--channel local` and a coarse `--runtime-class`
(`freetoken` / `openai-compat` / `colibri`). Never write model or provider ids into the ledger.
`brief` / `config` / `argv` do not call the model, so they do not record usage.

## Coach loop (host process-owner)

Local openai-compat writers need an active host coach — verify every artifact,
grounded feedback, never unsupervised finish. Playbook:
[coach-loop.md](references/coach-loop.md) (#6075).

## Folded: local-coding-delegate

[local-coding-delegate](../local-coding-delegate/SKILL.md) is a **compat shim**:
hardware probe stays there; local OpenCode / agency routing lives here. Prefer
this skill when the adopter names OpenCode or “local agents”.

## Related

- Guide: [local-agency.md](../../guides/local-agency.md)
- FreeToken: [freetoken](../freetoken/SKILL.md)
- Local OpenAI-compat: [local-openai-compat](../local-openai-compat/SKILL.md)
- OmniRoute (explicit cloud only): [omniroute](../omniroute/SKILL.md)
- Identity push-back: [identity-push-back.md](../../references/identity-push-back.md)
- Coach loop: [coach-loop.md](references/coach-loop.md) (#6075)
- Parent ROG Shell playbook: [parent-rog-shell.md](references/parent-rog-shell.md) (#6051)
- Parent ROG Shell assert smoke: [test_assert_parent_rog_shell.py](scripts/tests/test_assert_parent_rog_shell.py) (#6051)
