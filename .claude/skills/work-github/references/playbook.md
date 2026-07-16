# Work GitHub — playbook

A session-shaped methodology for taking one GitHub issue (or a small set of
open issues) from "filed" to "merged," doing as much of the middle
unattended as the user allows. Load `act-as-fable` alongside this skill and
every subagent you dispatch — this skill governs the *shape* of the session,
`act-as-fable` governs how each step inside it gets done.

## 0. Ground the scope before asking anything

Never ask the user a question you could answer yourself by reading the repo.
Before the first question:

- Read the target issue(s) in full (`gh issue view <n> --repo <owner>/<repo>`),
  including every linked/carried-over sub-item — issue bodies in this repo
  often list explicitly deferred items from a prior closed issue; treat those
  as first-class scope, not footnotes.
- Check for anything that already closes part of the scope: merged PRs,
  already-shipped commits referencing the issue number, a linked companion
  PR in another repo (`gh pr list`/`gh pr view` there too).
- List open issues/PRs across the repos actually in play (`gh issue list
  --state open`, `gh pr list --state open`) so "any currently open tickets"
  means a concrete, enumerated set, not a guess.
- Re-ground every sub-item's *file location* against the current code, not
  the issue text's possibly-stale line numbers or assumed files — grep for
  the symptom before assigning a sub-item to a file.

Only once you know the real, current shape of the work should you ask the
user anything.

## 1. Ask once, at the start, then go unattended

This repo's `AGENTS.md` defaults to one branch/worktree/PR per session,
sub-tasks as commits — which conflicts with a literal
"branch/code/test/push/pr/green/merge" loop over several independent items.
Do not silently pick an interpretation; do not ask piecemeal mid-session
either. Surface the real decisions in **one** `AskUserQuestion` call, grounded
in what step 0 found:

- **Branch/PR shape** — single branch + single PR + one commit per item
  (matches AGENTS.md's default; recommend this unless items are numerous and
  genuinely unrelated), vs. a separate branch+PR per item looped
  sequentially.
- **Merge authority** — auto-merge each PR once CI is green, vs. hold for
  manual review.
- **Any item whose scope is genuinely ambiguous** — e.g. an item the issue
  itself flags as borderline against the issue's own stated constraints
  (a "no new features" issue containing a deferred item that needs a real
  data-model change). Give the user the real tradeoff you found, not a vague
  "how should I proceed?".

Once answered, do not re-ask for the rest of the session unless you hit a
genuine blocker (a decision only the user can make, not a step you could
resolve by reading more code). The user asked for unattended execution —
honor that by not manufacturing more checkpoints than the one they granted.

## 2. Branch and track

- Fetch and prune, then branch fresh off `origin/main` (or the target repo's
  default branch — check, don't assume `main`; companion docs repos in this
  org default to `master`).
- Create a task list (one task per sub-item plus scaffolding tasks: docs/
  catalog update, memory commit, skill/graphify updates if applicable, final
  push+PR+merge). Update task status as you go — it's how you and the user
  both track a long unattended run.

## 3. Work items in dependency order, front-loading risk

Order sub-items so the riskiest/least-understood one goes first (per
`act-as-fable`'s "front-load the riskiest unknown") — if it invalidates an
assumption, better to learn that before three other items are built on top
of it.

For each item, decide dispatch shape:

- **Scout it yourself first** when it requires an architectural or
  data-model decision (e.g. "where does this state actually live, and can a
  fix live alongside an existing persistence contract without breaking it").
  Per this repo's Agent Hierarchy, that judgment call belongs to the main
  thread, not a subagent guessing at a spec you haven't written yet.
- **Delegate to a Sonnet subagent** once you can write a *detailed, concrete*
  spec: exact files, exact method/field names verified against the real
  code, the precedent pattern to follow (with a real file:line reference),
  what's explicitly out of scope, what tests to add and where, and the exact
  validation command to run. A vague spec produces a vague implementation —
  the spec is where your scouting work pays off.
- Every subagent prompt must open with: load `act-as-fable`, then follow it.
- **Sequential, not parallel, when file scopes overlap.** Two subagents
  editing the same file concurrently in a shared working tree will race or
  corrupt each other's edits — check each spec's file list against the
  others before deciding parallel vs. sequential dispatch. Only run
  independent-file items in parallel.
- Haiku is for low-risk mechanical edits, log/report summarization, and bulk
  repetitive triage — not for a spec that requires judgment calls mid-flight.

## 4. Review before you commit — every time

A subagent's self-report describes what it intended, not necessarily what it
did. Before committing any subagent's work:

- Read the actual diff (`git diff`), not just the report.
- Skim the new/changed tests for real assertions, not tautologies.
- If the subagent claims something empirically ("no screenshot impact",
  "nothing else broke") without showing the trace that proves it, either
  verify it yourself or send it back to prove it — don't accept an
  unverified empirical claim at face value, even from a subagent that
  otherwise did good work.

One commit per sub-item once it passes review, using this repo's normal
commit-message conventions (issue number in the subject line).

## 5. Docs, catalog, and screenshots — only where real

Update the user guide / `modular-era-feature-catalog.md` / companion docs
repo for whatever actually shipped. Regenerate screenshots only for panels
whose code you actually changed — a headless renderer will often show diffs
in unrelated panels from rendering-environment noise; leave those files
untouched and don't let unrelated churn into the diff. If a function's
externally-documented behavior changed, that's a companion docs-repo PR, not
a bundle into this repo's PR — open it separately and say so.

## 6. Learning Loop before wrapping up

Before the final push: route anything durable that surfaced mid-session —

- A durable fact, gotcha, or precedent worth remembering → commit it via
  this repo's memory mechanism (`.memory/`), not left implicit in the PR
  description.
- Something outside this issue's scope that you noticed but didn't chase →
  a new GitHub issue (search first, consolidate with an existing one if it
  overlaps) — never silently drop it, never silently expand scope to fix it
  now either.
- A skill or guidance file that misled you, or a genuinely new reusable
  procedure this session established → update the skill, not just a one-off
  mention in the report.

"Nothing durable surfaced" is a valid, honest outcome — say so rather than
manufacturing a memory entry to fill the step.

## 7. Push, PR, green, merge, compact

- Push the branch, open one PR per the shape decided in step 1, with a
  description that lists each sub-item and its commit.
- Wait for CI. If it fails, fix forward on the same branch — don't force-
  push over history the user might want to inspect, just add a fixing
  commit, unless the failure is trivially a fixup of the last commit itself.
- Merge only within the authority actually granted in step 1 — a "merge this
  PR" authorization is scoped to *that* PR, not to every PR the session
  touches. A companion PR in a different, live-publishing repo is a
  materially different blast radius; flag it for manual merge even under a
  broad unattended mandate unless the user's authorization explicitly
  covered it.
- After a merge, compact (or let the harness's `/compact` fire) so the next
  item — or the next session — starts with fresh context instead of a
  ballooning transcript.

## 8. Report

One clear summary at the end (or after each merge, if the user asked for a
report per item): what shipped, what was deferred and why, what's still
open (including anything filed as a follow-up issue or left for manual
review), and any surprise worth flagging. State verified facts plainly —
don't hedge on work you actually confirmed, and don't claim something is
done if you only believe it should be.
