---
name: self-improve
description: >-
  Use when running ChaosEngine Learning Session self-improve. Harness lessons
  are GitHub issues only. Product lessons may queue after delivery or on request.
---

# self-improve — ChaosEngine learning & adapting

Lean CE-native skill informed by Task Observer methodology
(Eoghan Henn / rebelytics, **CC BY 4.0** — see [LICENSE](LICENSE) and
[UPSTREAM.md](UPSTREAM.md)). Not a blind clone.

## When

- **Primary:** root-owned Learning Session after every final delivery (owner approval, publish done, final PR merged), automatic: lessons diff against the skills, one spec issue, ONE PR (`skip-release-notes`, auto-merge MERGE) babysat to merged.
- **Secondary:** explicit operator request mid-session.
- **Not:** every casual turn, and never the Learning Session PR's own merge.
- A lesson already shipped in the delivery pull request is not a new issue. Finalize with nothing durable.

If `assess` refuses `novel_success` under the one-incident rule, still
finalize: file or update the durable lesson tickets, then complete finalize
without inventing a second incident for the same success signal.

## Dual track

1. **Harness** — skills, hooks, MemPalace, Graphify, installer/doctor. GitHub
   issue only. Never a local queue and never chat.
2. **Product** — enhancements for the product under development (queued issues).

Details: [references/observation-taxonomy.md](references/observation-taxonomy.md).
Product track: [references/product-track.md](references/product-track.md)
(product issues/evals + ChaosGauge link; CLI/doctor/silent-verify over essay tickets).
Activation: [references/activation.md](references/activation.md).
Adopt/reject research: [references/research-adopt-reject.md](references/research-adopt-reject.md).
Roadmap: [../../references/self-improve-master-plan.md](../../references/self-improve-master-plan.md).
Research/explore isolation (harness): [../../references/context-firewall.md](../../references/context-firewall.md).

## How (wraps learning.py)

1. Classify each finding (harness vs product; category allow-list).
2. Write minimal fields only: `category`, `title`, `lesson`, `proposedChange`,
   `benefit`, `estimatedTokens`.
3. File a harness lesson as a GitHub issue. Queue only a product lesson
   through `learning.py --track product`.
4. Never auto-install skill patches; stage proposals for human/CI review.
5. "Nothing durable" is a valid outcome.
6. Refine this protocol when traces show skipped Learning Sessions, local
   writers reviewing their own diffs, or queue-only lessons that never became
   issues. Self-development has no cap.

Example:

A harness example is a GitHub issue, not a queue file. A product example:

```bash
python3 .chaos-engine/learning.py queue \
  --track product \
  --state .chaos-engine-state/learning \
  --upstream Owner/ExampleRepo \
  --candidate .chaos-engine-state/learning-candidate.json
```

## Metrics / verify (zero-LLM)

```bash
python3 .chaos-engine/learning.py metrics
python3 .chaos-engine/silent_verify.py session-start-budget
python3 .chaos-engine/retrieve.py heuristics --top 3
python3 .chaos-engine/significance.py list
python3 .chaos-engine/skill_compress_audit.py audit --skill self-improve
python3 .chaos-engine/meta_optimize.py review
python3 .chaos-engine/draft_skill_pr.py status
```

SessionStart stays locator-only; prefer CLI over MCP for the same job.
Mid-session: mark **significant** friction only via `significance.py` (soft
fail/deny hooks already write tiny state notes) — never Task Observer.
Skill bodies: `skill_compress_audit.py` proposes compress diffs only; never auto-apply.
Periodic: `meta_optimize.py review` (offline cadence — not continuous).
Draft skill PRs: `draft_skill_pr.py` opt-in only (default OFF; never auto-merge).
That covers the automated drafter only; the Learning Session's own lessons PR
runs the normal PR gates and auto-merges.
See [meta-optimize](../../references/meta-optimize.md) and
[draft-skill-pr](../../references/draft-skill-pr.md).

## Local smoke

```bash
# From a temp project with ChaosEngine installed, or the source tree:
python3 -c "from pathlib import Path; assert Path('chaos-engine/skills/self-improve/SKILL.md').is_file()"
# Confirm learning.py queue --track harness exits non-zero and writes no queue.json.
```


## Token retrospective

Helper: [`session_token_usage.py`](../../session_token_usage.py).

During the session, record coarse usage (no model/provider ids, prompts, or paths):

```bash
python3 .chaos-engine/session_token_usage.py record \
  --session-id "$SESSION_ID" --channel local --runtime-class freetoken \
  --prompt-tokens 1200 --completion-tokens 400
python3 .chaos-engine/session_token_usage.py record \
  --session-id "$SESSION_ID" --channel cloud --runtime-class host-session \
  --prompt-tokens 8000 --completion-tokens 1500
```

`learning_session.py finalize` attaches a privacy-safe local vs cloud token
summary with a ballpark USD estimate. Include that retrospective in the
user-facing Learning Session closing notes.
