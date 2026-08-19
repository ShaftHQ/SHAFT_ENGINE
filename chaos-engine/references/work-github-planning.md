# Work GitHub — planning and tracking

Read this half before work starts. Return to the [delivery playbook](work-github-playbook.md) for review, learning, CI, and merge.

## 0. Ground the scope before asking anything

Never ask a question the repository can answer. Before the first question:

- Read every target issue, including linked and deferred items.
- Check merged work and linked companion PRs that already cover scope.
- List open issues and PRs in every repository in play.
- Re-ground each item's file location with the current code, not stale issue line numbers.

First grounding table, recorded before the plan: issue state, closing PRs, and one live-file contradiction check.

| Check | Record |
| --- | --- |
| Issue state | Open or closed, plus `state_reason` |
| Closing PRs | Each PR GitHub reports as closing the issue, plus merged or open |
| Live-file contradiction | One current-file check that can falsify an issue claim |

A partial-slice merge (`Related to #N`, not `Closes #N`) must leave remaining-acceptance plus the next RED on the issue before the next writer starts.

When the owner names orchestration, grouped PRs, and kill-after-merge, that named grouping is the stream count. Do not rediscover closed children as new streams.

Issue-backed plans must be copied onto the GitHub issue before the first implementing commit. A host-local `plan.md` is not enough.

Only then ask about a decision the user alone must make.

## 1. Keep asking follow-ups until decision-ready; then go unattended

During planning, ground first. Never ask what the repository, retrieval stores,
or authoritative docs can answer. Then ask every material question, and keep
asking follow-ups until the plan is decision-ready. A plan that skipped those
questions is incomplete.

Ground remaining owner-only decisions: branch/PR shape, merge authority, and any
truly ambiguous scope. The default is one branch/worktree, linked subtask issues, and
one PR per related group; for genuinely disjoint work, offer a separate branch+PR per item looped sequentially.

After the owner approves the plan, go completely unattended. Do not ask the user
for implementation clarifications or for permission already granted. If execution
hits ambiguity, dispatch a consultant agent (read-only, independent) and take
its guidance. Absorb owner-initiated mid-flight requests.

### Mid-session realignment: named HALT conditions

HALT and ask when a new request changes the agreed branch, PR, or merge-authority
shape; cannot be grounded in current code (say what you searched); conflicts
with work in flight (surface the conflict and two options); needs merge
authority that was never granted; or a new request contradicts the approved
plan. Otherwise absorb the request into the owned plan.

## 2. Branch and track

- Fetch and prune, then branch from the checked default branch (`main` is not
  universal; companion documentation repositories use `master`).
- Track every sub-item plus docs, skill updates, the knowledge-store disposition,
  any structural change flagged for the existing maintenance owner, push, PR,
  and merge. Update task status as work proceeds; ordinary tasks never refresh a
  derived store.
- Keep one issue as a work stream; only dispatch independently scoped work.

## 3. Work items in dependency order, front-loading risk

Test the least-understood, highest-risk premise first. Scout architectural or
data-model decisions yourself. Delegate only a concrete spec that names the
files, precedent, exclusions, focused test, and validation command.

For consequential or multi-file work, follow the executable-specification
template in [consult-first](consult-first.md). Record the completed
executable-specification matrices on the target GitHub issue comment before the first
implementing commit.

## 3b. Tracking issue + one-issue-per-subtask (mandatory default for new work)

For every substantial request, analyze and plan before landing code, then open a
`Tracking: ` issue. If an existing issue later needs phasing, rename it to `Tracking: ` while retaining its title. Give every subtask a real linked issue and list each under `## Tracking` as a checkbox. Related subtasks share a PR whose body contains one
`Closes #N` line per completed subtask, never the tracker. On each merge, update
the tracker checkbox and post a progress comment with the shipped PR link and
remaining work, so the tracker is a current status page. After every checkbox is
checked, post a final summary comment and close the tracker in the same session.
Mark only file-disjoint concurrent tracker items `[P]`.

One issue owns one actionable problem or improvement. Do not consolidate separate
actions into a receipt, tracker comment, umbrella bullet, or existing issue: those
may link the standalone issue, but they are not its replacement.

### Example `gh` invocations

```bash
gh issue create --title "Tracking: <feature/program name>" --body "<summary and tracking checkboxes>"
gh issue create --title "<Subtask name>" --body "Subtask of #<tracker>. <scope>"
gh pr create --title "<group summary>" --body "Closes #<subtask-one>\nCloses #<subtask-two>"
gh issue edit <tracker> --body-file updated-tracker-body.md
gh issue comment <tracker> --body "Landed via PR #<pr>. Remaining: #<subtask>."
gh issue close <tracker>
```
