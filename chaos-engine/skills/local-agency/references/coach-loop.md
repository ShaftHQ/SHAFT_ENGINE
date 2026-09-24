# Local-agency coach loop (host ↔ openai-compat writer)

## When this applies

Any delivery that uses a READY local OpenAI-compat / llamacpp / FreeToken coder
through [local-agency](../SKILL.md) `dispatch.py`, especially small-context
coding checkpoints on a loopback OpenAI-compat server.

The **host process-owner** is teacher / mentor / coach / consultant. The local
model is a **mechanical runner** (and, with a locator-only CE brief, a bounded
drafter). Never treat a local turn as finished unsupervised.

## Roles

| Role | Owns | Does not own |
| --- | --- | --- |
| Host process-owner | Spec, RED contracts, verify, PR, merge, dual CE, Learning Session | Dumping full skill bodies into the local context |
| Local writer | Bounded patches / apply scripts / reproduce-from-example | Independent adversarial review; unattended multi-file design |

## Cadence (non-negotiable)

1. Dispatch one bounded ask.
2. Inspect the artifact on disk (diff, tests, syntax) before the next ask.
3. Feed **grounded** feedback: paste the failing traceback, empty-file proof, or
   wrong-import line — not vibes.
4. Re-dispatch until green locally, then open the PR.
5. Babysit required checks; coach the next fix the same way.

If you have not verified the file bytes, the turn is not done.

## Prompt shapes that work

### A. Mechanical apply (default for tool loops)

Orchestrator writes one idempotent script with exact paths. Local prompt is
**only** that executable line (see Mechanical dispatch in the skill). Commit
stays host-owned.

### B. Fixed-signature chat (when tool calls are fake)

Use this only when message content holds the tool JSON and `tool_calls` is
null. Exit 0 with no worktree change and no tool JSON stays on the bash retry
in the table below.

Ask for one method or one properties file. Temperature 0. Cap generation
small. Put the exact signature, or the exact property lines, in the prompt.
No skill body.

Host gate for a method: strip one fence, require the signature, reject a
wrapping class or `main`, splice only that method. Host gate for properties:
exact non-empty lines, no fence. Run the one contract test. On failure, the
next prompt is the failing assertion only.

### C. RED → GREEN codegen

1. Host lands failing contract tests first (or asks the local writer to add them
   from a precise spec, then verifies RED).
2. Ask the writer to make tests pass with the smallest patch.
3. On wrong free-form output (bad imports, empty `text`, syntax errors): stop
   retrying vague prompts. Switch to **worked-example reproduce**: show a
   minimal correct snippet or prior green file and ask for an equivalent under
   the new names/paths.

### D. Design / spec turns

Inject a locator-only system brief via `chaos-engine/ce_brief.py` when present
(`python3 chaos-engine/ce_brief.py --json`; #6067). Do not paste full SKILL bodies.
Host still reviews the design before implementation.

Follow the [design-turn contract](design-turn-contract.md). The first design
request is a skeleton that already contains the exact `CE_BRIEF_LOCATORS` line.
The writer must leave that line unchanged. Accept the draft only after
`design_turn_gate.py` citation passes. Reject vague product labels and specs
that name non-existent files unless marked `NEW`. Fold every durable reject into
this contract (keep it short).

## Locked ROG writer stack

Prefer:

```text
python3 chaos-engine/skills/local-agency/scripts/dispatch.py --prefer llamacpp resolve
```

Parent Shell must use `machineId` on the work machine (#6051). See
[parent-rog-shell.md](parent-rog-shell.md). FreeToken remains optional; do not
claim READY from a box probe of the work-machine ports.

## Failure classes → coach move

| Symptom | Coach move |
| --- | --- |
| EXIT 0, zero tool calls / no worktree change | Writer failure — re-run one bounded bash apply; do not praise |
| EXIT 0 and the tool JSON is only in message content (`tool_calls` null) | Stop OpenCode. Switch to fixed-signature chat in section B |
| `context_length_exceeded` | Shrink prompt + tool output; drop high reasoning; one command only |
| Bad imports / empty file / syntax error after free-form | Worked-example reproduce + runtime evidence |
| Inventory / reachability CI red | Host updates catalogs + skill links; refresh README inventory with the validator `--write` |
| Task child cannot see work-machine loopback | Stop Task writer; parent Shell `machineId` (#6051) |
| Design output cites fake CE locators / missing `CE_BRIEF_LOCATORS` | Reject. The first ask is the frozen `CE_BRIEF_LOCATORS` skeleton. Accept only after `design_turn_gate.py` citation |
| Spec names missing files without `NEW` | Reject; host pastes `rg`/`find` evidence |
| 7B writer copies `x; if ...` pseudo-code, drops docstrings, or regresses an existing function | Spec as real multi-line Python; after two failed rounds on an existing-function edit, switch to section A mechanical apply (#6161) |

## Pre-push tip preflight (#6164)

1. Run `python3 chaos-engine/skills/local-agency/scripts/tip_preflight.py` before every push (the `git push` guard runs it too).
2. Bandit B607: resolve argv0 with `shutil.which`; `# nosec B603` alone does not cover a bare executable.
3. README inventory: a new import in `chaos-engine/**/*.py` changes the inventory; run the validator `--write` in the same commit.
4. `.memory/` bodies touched outside `memory save --stdin`: `tip_preflight.py --rehash <sidecar>` in the same tip.
5. Known flake fingerprint (see [CI status economy](../../../references/ci-status-economy.md)): no tip.

Executor / Task prompts: `brief_path:` pointer plus the delta slice only; check with `executor_brief.py lint` (4096-byte cap). Failed CI logs: fingerprint first, at most 40 lines (`executor_brief.py log-budget`). See [tip-churn preflight](../../../references/tip-churn-preflight.md).

## After every delivery

1. Dual CE reinstall when harness files changed (work machine + agent canonical).
2. Learning Session even if `chaos-engine/` looked untouched — durable lessons
   belong in git-tracked `.memory/` / harness PRs, not only host chat memory.
3. Fold new durable coach moves back into **this** reference (keep it short).

## Related

- Skill: [local-agency SKILL.md](../SKILL.md)
- Guide: [local-agency.md](../../../guides/local-agency.md)
- CE brief builder: `chaos-engine/ce_brief.py` (#6067; land with CE-Brief P0)
- Design/spec contract: [design-turn-contract.md](design-turn-contract.md)
- Parent ROG Shell: [parent-rog-shell.md](parent-rog-shell.md) (#6051)
- Delegation / review stays host: [delegation.md](../../../references/delegation.md)
