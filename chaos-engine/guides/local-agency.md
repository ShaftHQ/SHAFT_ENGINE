# Local agency (OpenCode) guide

Operator-facing companion for [local-agency](../skills/local-agency/SKILL.md).

ChaosEngine does **not** install OpenCode. When you want OpenCode against
already-running local weights, CE routes to a READY loopback runtime and emits
ephemeral OpenCode config with `enabled_providers` limited to that runtime —
never `ft launch`, never a silent OmniRoute hop. OpenCode merges global config;
the allowlist (not `--pure`) is what keeps other providers out of that process.
`--pure` only disables external plugins.

## Official install (vendor)

- OpenCode: [https://opencode.ai/docs](https://opencode.ai/docs) (CLI install
  from upstream docs)
- FreeToken: [install.md](https://github.com/FlashML-org/FreeToken/blob/main/docs/install.md)
- Ollama / LM Studio / llamacpp: see [local-openai-compat guide](local-openai-compat.md)

## Skill helpers

```text
python3 chaos-engine/skills/local-agency/scripts/dispatch.py resolve
python3 chaos-engine/skills/local-agency/scripts/dispatch.py --prefer freetoken config
python3 chaos-engine/skills/local-agency/scripts/dispatch.py argv --prompt 'smoke' --workdir .
```

`config` prints JSON with `env.OPENCODE_CONFIG` pointing at a temp
`opencode.json`. Export that for one process only. Do not merge it into
`~/.config/opencode/opencode.json`. `argv` includes `--pure` and `--variant`
(default `medium`).



## CE project pointers (#6070)

`dispatch.py config` / `argv` preflight the worktree (`--project` or `--workdir`, else cwd)
for `AGENTS.md` and the install-generated `.agents/skills/chaos-engine/` skill adapter. Fail closed if either is missing.
`--pure` only disables plugins — install/activate ChaosEngine in the worktree first so the
pointers exist, then dispatch.


## Dispatch modes (#6073)

Default `--mode mechanical` keeps OpenCode on apply.sh-only prompts for small local
coders. Use `--mode design` when you want `dispatch.py` to attach the CE brief automatically
(design/spec turns). Mechanical remains the default for 7B tool loops.

## Parent CLI flags (#6087)

`dispatch.py` sets `allow_abbrev=False` on its parent `ArgumentParser`. Otherwise
`--mode` abbreviates to `--model` and a value like `design` is stored as the model
id. Keep that guard on any CE parent parser that defines short overlapping options
(for example `--mode` beside `--model`), and keep a regression test that parses
`--mode design` with `model` unset.


## Coach loop

When the host process-owner delegates to a READY openai-compat writer, follow [coach-loop.md](../skills/local-agency/references/coach-loop.md): verify every artifact, grounded feedback with runtime evidence, RED-first then worked-example reproduce after free-form fails, never leave the local model unsupervised (#6075).

## Mechanical dispatch

Small-context local models often exit 0 without tools, overflow on the next
turn after verbose logs, or miss gitignored trees via glob.

- Orchestrator writes one bounded runner; OpenCode prompt is **one command
  only** (no English like `git add product files` — #5996/#5997). Put exact
  `git add` paths inside the script.
- EXIT 0 with zero tool calls, Glob/Grep-before-bash, or overflow without
  mutation is a writer failure. Run the same command in the worktree; do
  not retry the oversized prompt.
- OpenCode bash timeout is **120s** (#5998). After a timeout kill of a
  Maven/Gradle apply.sh, re-run the same idempotent script from the
  orchestrator so the tree is not left half-applied.
- Use exact paths; OpenCode glob may skip gitignored trees (including Memory).
- Do not feed verbose unit-test logs into the next model turn.
- Advertised `context_length` is not usable KV. On `context_length_exceeded`,
  shrink prompt and variant; do not start `ft serve` from ChaosEngine.
- Size-class soak (`probe_hardware.py`: small / medium / large / refuse):
  knobs first. If still insufficient and class is medium or large, the
  operator may serve the next known-good coding MoE from vendor docs.
  `refuse`: do not recommend a larger checkpoint.

## Proven path (loopback + ephemeral config)

With a READY local runtime on loopback:

1. `dispatch.py resolve` → `state=READY` and a chosen local runtime
2. `OPENCODE_CONFIG=<ephemeral>` `opencode run --pure --variant medium …`
3. Durable `~/.config/opencode/opencode.json` hash unchanged


## ROG FreeToken bind (#6021 / #6051)

### Hard blocker (platform)

Grok Bot **Task / executor Shell has no `machineId` parameter** (confirmed in
Task tool schema; children report `hostname=cursor` / box). Parent
`ListMachines` + `Shell(machineId=…)` can reach ROG; Task cannot. FreeToken /
llama on ROG `127.0.0.1:1919` / `:8080` are unreachable from Task writers.

### Required workaround (harness)

Until Grok Bot exposes `machineId` on Task → executor Shell/Read/AwaitShell:

1. **Process-owner is the only ROG writer** for FreeToken / OpenCode / local-agency
   delivery. Use parent Shell with the connected ROG `machineId` and checkout
   `/media/mohab/OS/Users/Mohab/IdeaProjects/SHAFT_ENGINE`.
2. **Do not** dispatch ROG implementers via `Task` expecting them to bind ROG.
   Task may still do box-safe work (docs on `/workspace`, GitHub API) but must
   **not** claim ROG/FreeToken delivery.
3. Gates (fail closed on box):
   - `python3 chaos-engine/skills/local-agency/scripts/require_rog_freetoken.py check`
   - `python3 chaos-engine/skills/local-agency/scripts/assert_parent_rog_shell.py`
   - `dispatch.py resolve --prefer freetoken`
   Override only with `CE_ALLOW_BOX_LOCAL_AGENCY=1` (or `CE_ROG_CHECKOUT`).
4. When a Task hits the gap, it must report verbatim:
   `HARD_BLOCKER: Task Shell has no machineId` and stop — parent re-runs on ROG Shell.

See issue #6051 (platform + harness) and living lesson under OmniRoute references.



## Cost and savings reporting

After a delivery that used local loopback inference, the process-owner final
report (and Learning Session notes) must include:

1. **Actual cost** — USD + EGP for any cloud tokens used in the same delivery.
2. **Avoided cloud spend** — local prompt + completion tokens × the published
   per-1M input/output rates of the cloud model that would otherwise have done
   the writer work, converted to EGP with the owner's FX.

Record local usage during the session (or let `dispatch.py chat --session-id …` do it when the
runtime returns `usage`; never pass model/provider ids into the ledger) (#6069):

Record local usage during the session:

```bash
python3 chaos-engine/session_token_usage.py record \
  --session-id "$SESSION_ID" --channel local --runtime-class openai-compat \
  --prompt-tokens N --completion-tokens M
```

Do not invent rates or FX. Cite the rate source and FX date in the report.
Local channel actual cost is $0 unless a paid local host was used.



## Locked work-machine stack (adopter default)

When the work machine has a proven OpenAI-compat loopback coding runtime:

1. Prefer `dispatch.py --prefer openai-compat` (not FreeToken) for local writers.
2. Parent Shell with `machineId` on the work machine — Task children have no
   `machineId` (#6051).
3. Keep a single user systemd unit for the loopback server so it returns after
   reboot; do not leave experimental MoE / alternate checkpoints loaded by
   default.
4. Record local token usage and report avoided cloud spend on delivery close
   (see Cost and savings reporting above).

FreeToken remains an optional companion skill in-tree; it is not the default
writer on hosts that locked OpenAI-compat.

## Session agents vs local agency

Default orchestrator labor stays on host session subagents / Task. Use this
guide only when the adopter asked for OpenCode (or peer) against local
runtimes. If no local runtime is READY, say so; do not auto-route to OmniRoute
unless asked.


## Colibri

Optional frontier MoE peer on `127.0.0.1:8000`. Ranked after FreeToken and OpenAI-compat peers. `--prefer colibri` when requested. See [colibri.md](colibri.md).
