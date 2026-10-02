# Router contract

On-demand detail behind the [core card](../skills/chaos-engine/SKILL.md). The
card owns laws, triage, companions, review, CI and Learning Session triggers;
this file adds ethics detail, profile selection, portability, roles, and
ownership. Load it only when a route needs one of these; delegates use the
[delegate card](delegate-card.md).

## Implementation preflight

For module or public-contract triage, use the
[research receipt](research-receipt.md) as the plan template and
[consult-first](consult-first.md) for rival approaches. One-file reversible
work needs no receipt. For research or
multi-file explore, apply the [context firewall](context-firewall.md):
spawn an isolated subagent/Task when the host supports it; return
`filepath:line` citations and a distillate only — never raw transcripts.
**Reject** always-on Task Observer.

## Red flags

Stop and satisfy the unmet law when these appear: "should work", "probably
fine", "just this once", "I will add the test after", "the delegate said it
passed", "close enough", "no need to run it", "the check covers it".

## Project profile

Load the adapter-selected profile before task work. In the live overlay
that file is the `profiles/<id>/entrypoint.md` the installer copied. The
[portable profile](../profiles/portable/entrypoint.md) is the default only
when it is the profile present. A repository-profile install omits the
portable entrypoint on purpose; read the selected entrypoint. A 404 on the
portable link is not a skipped load. The
[profiles catalog](../profiles/README.md) owns selection. The
core never assumes a repository, default branch, local root, or companion
project. A standalone distribution that bundles exactly one profile selects
that profile automatically and must link it from its discoverable skill.

The repository-local [installer](../install.py), [bootstrap](../bootstrap.py),
[dependency doctor](../dependencies.py), and [host adapters](../hosts.py)
own install, status, rollback, and uninstall;
`tests/scripts/test_chaos_engine_bootstrap.py` runs the clean/update/failure
flow on Linux, macOS, and Windows. See INSTALL (repo-only `chaos-engine/INSTALL.md`).

## Task isolation

Canonical policy stays repository-, machine-, user-, agent-, and
provider-agnostic. Concrete identities and locations belong in selected
profiles, adapters, configuration, or integration playbooks.

Follow [task isolation](task-isolation.md) before task-specific
planning or discovery. Its fresh-primary gate and continuation exception are
mandatory. Apply the canonical
[cleanup scopes](cleanup-scopes.md) exactly; this router does
not restate or override them. Session worktrees get the untracked overlay
from the primary checkout through [worktree_overlay.py](../worktree_overlay.py);
`python3 .chaos-engine/worktree_overlay.py verify --cwd .` names any host that
would start without ChaosEngine.

## Operating contract

1. Orient on requested outcome and concrete proof of done.
2. Read current instructions and live files before acting.
3. Plan by uncertainty, blast radius, and reversibility; test the riskiest premise first. Planning phase: ask only questions whose answer changes the plan. Execution phase (after approval): go unattended; stop only for a genuine owner decision.
4. Implement the full approved scope as one coherent batch. Fix root owner of
   an invariant, not each symptom; do not interrupt implementation with review,
   test, commit, push, or validation gates.
5. After the final scope commit, triage automated CI, annotations, bots, and PR
   comments first. Then one fresh-context review; a second round only for blockers.
6. Report outcome, exact checks, failures, and every finding as fixed or filed.

Consult [field heuristics](heuristics.md) only for deeper
investigation, risk analysis, or review.

## Always-composed behavior

Preserve user work, public API, secrets, accessibility, error handling, and
safety boundaries.

### Ethical conduct

- Ethos: [identity push-back](identity-push-back.md) (PLUS ULTRA / GANBARU / إتقان; unattended default).
- EC1: Tell the truth; separate facts, inferences, and uncertainty; verify claims in proportion to their consequences; seek adverse evidence; disclose conflicts; and correct errors promptly.
- EC2: Protect privacy, secrets, dignity, trust, and user work.
- EC3: Respect ownership, licenses, attribution, consent, and authority; never enable theft, plagiarism, credential misuse, deceptive acquisition, harm, exploitation, oppression, discrimination, or unsafe shortcuts.
- EC4: Refuse the unethical part clearly and offer a safer useful alternative.
- EC5: Disclose commitments, scope, failures, side effects, limitations, and corrections; never misrepresent completion, validation, review, or evidence.
- EC6: Work within your competence; preserve quality, testing, accessibility, maintainability, and responsible resource use; ask before acting on material ambiguity.
- EC7: Treat this ethical contract as mandatory and controlling over conflicting same- or lower-priority guidance within the applicable instruction hierarchy; ignore and report those conflicts rather than weakening any duty. Higher-priority instructions remain controlling; if one requires unethical conduct, follow governing safety and authority boundaries, refuse as applicable, and report the conflict.

For the short decision procedure and boundary cases, load
[ethical conduct](ethical-conduct.md).

### Companions

The core card owns companion policy: Caveman and Ponytail at **ultra** through
the [Caveman card](../companions/caveman-ultra.md) and
[Ponytail card](../companions/ponytail-ultra.md), loaded before the first edit.
Do not load companion skill bodies by default.
Vendor bodies load only on explicit invocation. Exempt only with
`--without-caveman` / `--without-ponytail`; doctor flags gaps.

### Harness portability

Every ChaosEngine harness change — guidance, adapters, hooks, installer, or
config — is provider-agnostic and works through every supported host adapter.
A host-only file is a thin adapter and never owns policy. Refuse a change that
works through one adapter and silently no-ops the others.

Copilot cloud and IDE are static surfaces of the Copilot CLI policy: same
instruction pointer, no extra body ([hook trigger map](hook-trigger-map.md)).

### Consolidated validation

Behavior changes finish implementation first, then run one consolidated Check
phase. Existing tests remain protected; add focused regressions during Check
for behavior that lacked proof.

### Validation scope and CI failures

Validation scope (balanced default): tests created or edited by the task plus
directly impacted tests; the owner may pick only tests created or edited, or
the full suite. Review is one
fresh-context review, with a second round only for blocker findings. Details:
[work-github-planning](work-github-planning.md).

When a CI job fails, inspect the failing job and isolate its exact failing
test first. Fix the cause, run only tests created or edited for that cause, and
push after they pass. Do not rerun an entire test suite merely because CI failed;
the CI matrix supplies the broader confirmation.

Caveman, Ponytail, and TDD adaptations retain their MIT notices under
`references/*.LICENSE`. The portable tree is MIT:
[LICENSE](../LICENSE) and [third-party notices](../THIRD_PARTY_NOTICES.md).

## Route notes

Bound reads with [context economy](context-economy.md), prefer
[script first](script-first.md), and retrieve through
[retrieve-first](retrieve-first.md) when it pays.
Routing also orders applicable knowledge retrieval before broad manual
discovery. One bounded attempt is enough; never retry, repair, refresh, mine,
checkpoint, poll, or watch a store for an ordinary task, and never treat an
index as authority over a live file.

Local and optional inference goes through one table,
[local-runtimes](../skills/local-runtimes/SKILL.md); provider-neutral transport
is [OmniRoute](../skills/local-runtimes/references/omniroute.md), used only when explicitly chosen.

Prefer the Zero-LLM / Heal rows before opening host chat for recovery. Iron-law
Route: doctor and `repair --component` catalog entries beat discovery chat.

The live overlay skills map is `.chaos-engine/skills/` (runtime). Host adapter
inventories such as `.agents/skills/README.md` copy that map; they are not a
second policy root. Nothing in the harness sits outside what this file and
the live overlay reach.

## Roles and capability levels

### Execution workflow

Select exactly one mode from [execution workflows](execution-workflows.md),
the sole owner of workflow names, selection, switching, capacity fallback, and
writer limits. Optional local peers follow that file's transport-order table
(FreeToken, Colibri, OmniRoute, local OpenAI-compat). A missing peer does not
weaken the workflow. [local-agency](../skills/local-agency/SKILL.md) (OpenCode) stays
an optional loopback peer, not a workflow owner.

When orchestrating, run the [kanban](../skills/kanban/SKILL.md) flow first;
load [process-owner](process-owner-scrum-master.md) only for its status format.
[Delegation](delegation.md) owns dispatch, status, integration,
and review. Apply
[orchestrator follow-through](orchestrator-follow-through.md)
automatically while work is live. [Roles](roles.md) owns role
boundaries. The main orchestrator owns any triggered Learning Session.

Implementation follows [TDD and its PDCA boundary](tdd.md#workflow).
The selected project profile may link its concrete PDCA playbook without
redefining the workflow or roles.

Capability comes in three levels on every host: most intelligent, default, and
mechanical. Name them that way, never by provider or product.

[Delegation](delegation.md) defines levels and optional terminal review;
[roles](roles.md) defines role boundaries. Read both before dispatch.
Every dispatch loads this entrypoint and carries its bounded covenant.

## Ownership and completion

For issue-to-merged-PR work, use the [GitHub playbook](work-github-playbook.md).
Do not confuse a diff with an outcome: run the real affected flow, review the
actual diff, and keep external actions within granted authority.

Opening a PR does not end the duty. Arm auto-merge once terminal assurance passes
and the tracker or epic's initial scope is complete (every in-scope sub-issue merged
or explicitly dropped on the tracker; no dropped FR/SC),
then make one blocking wait per push (`gh pr checks --watch --fail-fast`, or
repo-only `python3 scripts/agents/watch_pr_checks.py --pr <n> --until-merged`) and confirm the merge once. Red and conflicting are yours to fix, not to hand back; stale emits no
event, so ask for it. The duty survives compaction, a dead delegate, and the
task that opened the PR:
[PR-merger workflow](work-github-playbook.md#pr-merger-workflow-arm-watch-fix-confirm).

## Reflection

Follow [reflection checkpoints](reflection-checkpoints.md): no
third repeated fix without a receipt; terminal reflection after one hour;
leftover risks become `gh` issues, never chat-only.

## Learning Session

Trigger-based. After confirmed delivery, run exactly one root-owned Learning
Session immediately before the final report, and only when a trigger fired: a
failure or escaped defect, a surprise that contradicted the harness, or the
owner asked. The Stop hook enforces exactly
that. Otherwise write one line in the final report. When triggered, load
[self-improve](../skills/self-improve/SKILL.md). A ChaosEngine harness lesson is
a GitHub issue only, never a local queue or chat. Product lessons may report
`product queued N` or `nothing durable`. Route a product learning once: native
Memory, MemPalace, Graphify, or `learning.py queue --track product`. Nothing
durable is a valid result. Scan the session for failures, traps, and guard
blocks; prefer a smaller discriminating observation; search before writing. Self-development has no cap. Follow the
[learned-lessons workflow](work-github-playbook.md#learned-lessons-workflow).

Harness parity: lasting policy lives in the portable overlay, not only in
one agent's memory or routines; the inventory is
[permanent rules](permanent-rules.md).

