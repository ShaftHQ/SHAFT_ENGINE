# Mechanical dispatch (small-context local models)

Treat a READY local coder as a **mechanical runner**, not a designer.
Never use it for independent adversarial review. Review stays a new instance of
the host session model (see [delegation](../../../references/delegation.md)).

1. Orchestrator writes one bounded command (a short script with exact paths).
   The OpenCode prompt is **only** that executable line (e.g.
   `bash <abs-dir>/apply.sh`). No extra English nouns such as
   `git add product files` — those trigger Glob/Grep then
   `context_length_exceeded`. Keep `git add` of exact paths
   **inside** the script; commit stays orchestrator-owned.
2. OpenCode invokes **exactly that one bash command**. Multi-step specs in
   chat are writer failures: EXIT 0 with zero tool calls is writer failure,
   not success. The same failure is EXIT 0 when the tool JSON is only in
   message content and `tool_calls` is null: stop OpenCode and switch to
   fixed-signature chat in [coach-loop](coach-loop.md) section B.
   Glob/Grep of prompt English before the named bash command,
   or overflow with no worktree mutation, is writer failure — run the same
   command in the worktree; do not retry the oversized prompt.
3. OpenCode bash tool wall-clock is **120s**. Build tools apply
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
   usable KV. Do not OpenCode-`Read` a 200-line source file; pass the exact
   line change. Soak on this class of host: keep the prompt well under 8k
   combined prompt+generation.
7. Size-class soak: prove the current READY checkpoint with these knobs.
   Only if quality is still insufficient **and**
   [`probe_hardware.py`](../../local-coding-delegate/scripts/probe_hardware.py)
   returns `medium` or `large` may the operator serve the next known-good
   coding MoE from **vendor docs**. `refuse`: do not recommend a larger
   checkpoint. ChaosEngine never downloads weights or starts serve.

Example (session only; model id is not a git pin):

```text
python3 .chaos-engine/skills/local-agency/scripts/dispatch.py --prefer freetoken config
# export OPENCODE_CONFIG from the JSON env map for one process only
OPENCODE_CONFIG=<ephemeral-path> opencode run --pure --variant medium --dir '<worktree>' '<one command>'
```

Hardware size-class hint:
[`local-coding-delegate/scripts/probe_hardware.py`](../../local-coding-delegate/scripts/probe_hardware.py)
(`small` / `medium` / `large` / `refuse`).
