# Orchestrator bootstrap

Read this when you assume the orchestrator role — on "act as mohab", or on
any session where you hold the main thread. It is the mechanics; `SKILL.md`
holds the binding rules and is the only home of the charter itself.

The goal of the bootstrap is a *queue you can defend*, not a fast start.
Every step below is evidence-gathering; none of it is implementation. You
never write production code in this role, at any point, for any reason —
"it's only one line" is the exact rationalization the owner rule exists to
stop.

## 1. Establish scope

Name the repositories in play before listing anything, and say which you
picked. Default is the current repo. Widen only on evidence:

- `SHAFT_ENGINE` — the framework and this harness (`ShaftHQ/SHAFT_ENGINE`).
- `shafthq.github.io` — the docs site; a function change needs a docs PR, so
  a framework ticket often implies a paired docs ticket.
- Anything the owner names explicitly this session.

Do not sweep every repo the account can see. An unscoped sweep produces a
queue nobody asked for and burns the usage window before real work starts.

## 2. Gather live state

Session-entry conventions (fetch/prune, fresh `ChaosEngine/*` branch or
worktree from `origin/main`, conflict handling, tracker + subtasks) are
already binding in `AGENTS.md` "New Task Flow" — follow them there, they are
deliberately not restated here. On top of them, gather:

```
gh issue list --state open --limit 100 \
  --json number,title,labels,assignees,updatedAt
gh pr list --state open --json number,title,isDraft,mergeable,statusCheckRollup
git worktree list
gh run list --branch main --limit 5
```

Read the results before ranking them. An issue title is a claim, not a
finding: open the ones you intend to queue, and check for an existing PR or
worktree already covering the work. Queueing a ticket that a live branch
already solves is the most common orchestration waste.

Consult `memory` and `mempalace` for prior context on any area before
grepping it — never grep for what a store already knows — then verify
against live code with `rg`, since stores reflect what was mined.

## 3. Build the queue in priority order

Owner-binding order, highest first:

1. **In-progress work** — anything with a live branch, worktree, or agent.
   Land it before starting anything new.
2. **Open PRs** — red checks, conflicts, review comments, stale-but-green.
   A PR sitting unmerged is finished work earning nothing.
3. **Bugs** — defects in shipped behavior.
4. **Red `main`** — treat as a bug of the highest severity when present.
5. **Direct owner asks** — always ticketed first, never worked untracked.
6. **Backlog** — enhancements, cleanups, docs.

Completeness outranks parallelization. Prefer finishing and merging over
opening another front, and wind down to a fully-landed state rather than
starting large work late in a usage window.

## 4. Check conflicts, then size the fan-out

Before parallelizing anything, diff the **file scope** of each queued item
against every other queued item *and* every in-flight agent. Two agents
editing the same file in one checkout is a corrupted branch, not a merge
conflict — it is the failure the worktree rule exists to prevent.

- Overlapping file scope → sequence them, or merge the first before
  branching the second.
- Disjoint file scope → they may run in parallel.
- Each parallel agent gets its **own worktree**; never two writers in one
  checkout.

Budget: 2–4 concurrent level-1 agents, hard cap 4, never exceeded. Fill the
budget when work is genuinely independent; sequence when it is coupled. Each
level-1 agent may itself fan mechanical, spec-exact, or bulk slices out to
Haiku level-2 delegates.

## 5. Ticket before dispatch

Multi-tier work opens its tracking issue **before** any agent launches: one
umbrella issue with a checkbox per subtask, plus one real linked issue per
subtask. PRs close subtask issues; the tracker gets a progress comment on
every close and a summary comment when the last one lands. Mechanics live in
`AGENTS.md` "New Task Flow" and `work-github` Section 3b.

## 6. Dispatch bounded specs only

Every dispatch is a **bounded spec with a definition of done** — the files in
scope, the behavior that must change, and the check that proves it. Never
hand a delegate an open-ended loop: `/work-github` and `/loop` are
orchestrator-only, `coder`/`reviewer`/`tester` are required to refuse them in
any phrasing, and `guard.py` R7 blocks the invocation structurally. If a
queued item needs that shape of work, you drive the `work-github` playbook
yourself on the main thread.

Route by task shape: `coder` implements, `reviewer` verifies (read-only),
`tester` reproduces and proves. All delegates run at HIGH effort, stated
explicitly in the prompt.

Every dispatch prompt must contain, verbatim:

- Load `act-as-mohab`, then `test-driven-development`. Both bind.
- Step 1 before any code: fetch and branch fresh from latest `origin/main`.
- The **Subagent covenant** from `SKILL.md`, embedded in full.
- The bounded scope, and the instruction to report adjacent findings rather
  than fix them.
- The instruction to embed this same covenant when sub-delegating to Haiku.

## 7. Stay available, then verify

Hold the consult duty: when a delegate surfaces an architectural question,
you decide it and hand the decision back so it can proceed. Never go so deep
in one thread that a new owner directive has to wait.

Run the 20-minute stall watch on every delegate and every long-running
command, and give real support at the check-in — a solved sub-problem, a
decision, a re-spec — never a bare "status?".

Review returned work like a hostile reviewer before building on it; triage
findings into decision_needed / patch / defer / dismiss, and let your
severity call override the delegate's, since delegates see less context.
Synthesis, integration, and final verification stay on the main thread.

## 8. File what you found

Every adjacent finding — yours or a delegate's — becomes a real
`gh issue create` in the same session. Searching first and consolidating
duplicates is part of filing; a chat mention is not filing it. Deferred or
out-of-scope language in a PR description is a filing obligation, not a note.

Close the loop before wrapping: `memory remember` durable decisions and
gotchas as they are confirmed, flag a graphify refresh if the repo structure
moved, and fix or add a skill when guidance misled you. "Nothing durable
this session" is a valid outcome — say so explicitly.
