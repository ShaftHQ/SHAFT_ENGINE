---
name: ci-cd
description: >-
  Use when PR merge waits are long or CI/CD is being designed, audited, or sped
  up: targets, PR/main/nightly tiers, opt-in labels, roll-forward, merge queue,
  release and privileged workflows.
license: MIT
---

# CI/CD tiers and PR merge time

Learned in a large monorepo: the median open→merge time was about 30 minutes, and the gate took 6–11 minutes.
Fix the loop first, then the slowest leg.

## Targets (evidence)

- Merge-blocking gate ≤ 10 min p90: Fowler's 10-minute build.
- Open→merge median ≤ 15 min for auto-merge PRs. DORA elite lead time is < 1 h, and the LinearB elite merge time is < 1 h.
- Small changes: elite PR size is < ~90 changed lines. Google's median review is < 4 h with one reviewer.

## Measure first

1. Gate wall-clock: `gh run list --workflow <gate> --event pull_request --status success -L 40 --json createdAt,updatedAt`. Report p50, p90 and max.
2. Open→merge: `gh pr list --state merged -L 30 --json createdAt,mergedAt`. Report the median.
3. Runs per PR and the failure share. Re-push loops usually cost more than slow legs do.
4. Per-job durations plus queue time. The critical path is the slowest leg, not the sum.

## Three tiers

| Tier | Runs | Rule |
|---|---|---|
| PR (blocking) | path-filtered affected-module build + unit tests, lint/guards, required governance | only what MUST block; light |
| Push to main | the same path filters (tests for unrelated modules never run), never cancelled, plus slow OS/E2E legs | a red leg opens or updates one tracker issue, which auto-closes on green |
| Nightly/scheduled | full cross-OS acceptance, live/external suites, security scans, EAP verifiers | tracker issue, no `Closes` keyword |
| Release | full verify, always | never weakened |

Move a leg off PR only when all three hold:
- it is > 5 min or is the critical path;
- it caught no PR-only regression in its last ~20–30 runs;
- the main tier runs it.

Keep a live-label opt-in such as `ci:<leg>`, plus `ci:full`, and re-run the gate when the label is added. Read labels from the API, because re-runs replay the old event payload.

## Quality layers are never removed

Tests, static analysis, coverage and review may be rearranged or added, but never removed.
The one exception is a layer that repeated, evidenced analysis proves an utter waste.
Moving a layer, for example from PR to main or from blocking to reporter, must keep equal or better coverage:
- a blocking in-repo equivalent;
- findings resolved before merge;
- a tracker issue on main.

## Never auto-revert

Main rolls forward. A failing post-merge leg opens or updates one rolling issue (P0, assigned to the owner), and the next green run closes it. Do not open revert PRs automatically.

## First push green

- Run the cheap CI tests locally before every push: workflow README/inventory, timeout, path-filter and PR-body validators.
- Check the PR body against the repo's required sections, and name a targeted test, before creating the PR.
- A body edit does not re-run a PR-body guard that reads the event payload. Push a commit instead.
- Validate every third-party CI config (coverage, analysis) against the vendor's validator in pre-push and CI. Some vendors silently ignore a whole file over one unknown key, and fall back to defaults.
- Coverage carryforward reads the vendor's record of the git parent. An old branch whose parent chain lacks the full flag set never goes green. Move its commits to a fresh branch from main, and make every main push upload every flag.
- A gate that reads an external status, such as Codecov, must wait for a settled verdict (an unchanged title and completion time across polls). The vendor recomputes per upload, so a mid-upload red is not final.

## Release and privileged workflows

- New release safety is additive: a hard gate before publishing, and `continue-on-error` for anything after publishing, such as attestation or SBOM upload, so it can never strand a published release.
- An API-compatibility baseline must be the published artifact downloaded as a file. Comparing against the same version number resolves to the reactor jar, so the build compares the jar with itself. Give the comparator any missing optional dependencies precisely; never set a global ignore-missing-classes, which hides real breaks.
- `pull_request_target` and `workflow_run` jobs never check out or run PR code. Use two workflows instead: the unprivileged `pull_request` job computes a result and uploads an artifact. The privileged `workflow_run` job re-reads the PR from the API, validates the artifact against an allow-list, applies it as data and pushes.

## Delivery order, queue, stacks

- One PR at a time: babysit it to merged, then open the next. With a merge queue, `gh pr merge --auto --merge` prints "merge strategy is set by the merge queue" but may still enable auto-merge. Confirm `autoMergeRequest` is set. If the PR is green and not queued, call the GraphQL `enqueuePullRequest` mutation: its error names the exact blocker, such as a cancelled or expected required check.
- A required check must never share a concurrency group across runs. GitHub keeps only one pending run per group, so a burst of PR events cancels the queued one, and a cancelled required check blocks the queue. Use a per-run group (`github.run_id`) and no cancellation.
- Merge queue: add `merge_group:` to every workflow that reports a required check, and run the PR tier there. External apps that do not report on `merge_group` must leave the required list first.
- Stacked PRs: run cheap checks on every layer, and expensive legs only on the lowest unmerged layer or the top of the stack (`github.event.pull_request.stack`).

## Report

Before/after p50/p90 gate time, the open→merge median, the PRs merged, and the admin changes left for the owner (rulesets, merge queue).
