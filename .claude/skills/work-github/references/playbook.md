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

This repo's `AGENTS.md` defaults to one branch/worktree per session with
sub-tasks as commits, grouped into one PR per group of related subtasks (Sec.
3b below) — not the older single-PR-per-session convention it supersedes.
Do not silently pick an interpretation; do not ask piecemeal mid-session
either. Surface the real decisions in **one** `AskUserQuestion` call, grounded
in what step 0 found:

- **Branch/PR shape** — one branch, one tracking issue, one issue per
  subtask, and one PR per group of related subtasks (the default per Sec.
  3b) vs. a separate branch+PR per item looped sequentially, for cases
  genuinely too disjoint to group.
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

### Mid-session realignment: named HALT conditions

Per act-as-fable's Ownership, a new ask mid-run joins the same owned plan —
absorb and keep going, in most cases. But absorbing on missing evidence is
how a session drifts silently off the owner's actual intent. HALT and ask,
by name, when any of these hold (adapted from bmad-method's
`bmad-correct-course/checklist.md` halt-condition pattern, MIT-licensed —
not its PRD/epic machinery, which doesn't apply here):

- **HALT if the realignment changes the branch/PR/merge-authority shape
  agreed in step 1.** A broad "keep going" for the original scope is not
  authorization for a different shape — re-run `AskUserQuestion` for the
  delta, don't silently reinterpret.
- **HALT if you cannot ground the new ask in real code.** No matching
  symptom, file, or prior art after an honest search means you'd be
  building on a guess — say what you searched and ask, don't proceed on
  inference.
- **HALT if the new ask conflicts with work already in flight** (same file
  scope, contradictory requirement, or it obsoletes a sub-item mid-commit).
  Surface the conflict and the two options; don't quietly drop either side.
- **HALT if merge authority for the newly-added scope was never granted.**
  Authorization doesn't transfer (act-as-fable, When judging risk) — an
  auto-merge mandate for the original item set doesn't extend to scope
  added after the fact.

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
- **Delegate to a Sonnet L1 agent** (`coder`; `reviewer`/`tester` for their
  lanes) once you can write a *detailed, concrete* spec: exact files, exact method/field names verified against the real
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

## 3b. Tracking issue + one-issue-per-subtask (mandatory default for new work)

Owner directive (2026-07-20): this is the binding default for **every** new
substantial work request from the very start — not only for an issue that
later turns out to need multiple phases. Analyze, plan, and architect the
request *before* any code lands, then open the tracking issue. It supersedes
any older "1 PR per session" framing found elsewhere in this repo's docs: a
session may open several grouped PRs, one per group of related subtasks.

- **Title prefix**: the tracking issue's title starts with `Tracking: `
  (rename an issue when it turns out to need phasing, keep the rest of the
  title).
- **Subtask issues**: every subtask gets its own real, linked GitHub issue in
  the repo the work lands in — never just a checkbox with no ticket behind
  it. The tracking issue's body holds a `## Tracking` section with one
  checkbox per subtask, each linking its issue (`- [ ] Subtask N — <name>
  (#<subtask-issue>)`).
- **Grouped PRs, multiple `Closes #N`**: group related subtasks into one PR
  rather than opening one PR per subtask. Each PR body carries one
  `Closes #N` line per subtask issue that PR completes — never the tracking
  issue itself, and never a plain `#N` reference (see the "closing keywords"
  memory constraint). A session doing several unrelated groups of subtasks
  opens several such PRs.
- **On each subtask close**: check that subtask's box in the tracking
  issue's body and post a progress comment on the tracking issue summarizing
  what shipped (PR link) and what remains — the tracking issue must read as
  a current status page, not a stale plan.
- **Close-out**: when the *last* subtask closes and every checkbox is
  checked, close the tracking issue itself in the same session, with a final
  summary comment — don't leave it open awaiting a human close.

### Example `gh` invocations

Open the tracking issue with a checkbox list (subtask issues don't exist yet,
so the checkboxes start unlinked and get backfilled with issue links once
each subtask issue is filed):

```bash
gh issue create --title "Tracking: <feature/program name>" --body "$(cat <<'EOF'
## Summary
<one-paragraph plan/architecture for the whole request>

## Tracking
- [ ] Subtask 1 — <name> (#TBD)
- [ ] Subtask 2 — <name> (#TBD)
- [ ] Subtask 3 — <name> (#TBD)
EOF
)"
```

File one real issue per subtask, then backfill its number into the tracking
issue body (`gh issue edit <tracker> --body-file -` or a targeted PATCH via
`gh api`):

```bash
gh issue create --title "<Subtask 1 name>" --body "Subtask of #<tracker>. <scope>"
```

Open a PR for a group of related subtasks, closing every subtask issue that
group completes:

```bash
gh pr create --title "<group summary>" --body "$(cat <<'EOF'
## Summary
- <what this group of subtasks does and why>

Closes #<subtask-1>
Closes #<subtask-2>

## Test plan
- [ ] <check>
EOF
)"
```

After that PR merges, check the completed subtasks' boxes and post the
progress comment on the tracker in the same session:

```bash
gh issue edit <tracker> --body-file updated-tracker-body.md   # boxes now [x]
gh issue comment <tracker> --body "Landed #<subtask-1>, #<subtask-2> via PR #<pr>. Remaining: #<subtask-3>."
```

When the last subtask closes, close the tracker itself with a final summary:

```bash
gh issue comment <tracker> --body "All subtasks complete. <final summary>."
gh issue close <tracker>
```

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
