# Iterative flow: one item at a time

Always-on rule for every task, not only design. It sets the cadence around
the [multi-agent PDCA loop](multi-agent-pdca.md), which defines each round.

## Rule

1. Deliver iteratively and incrementally. Never run the whole batch through
   stage 1, then stage 2 (waterfall).
2. WIP limit: 1 item per owner–reviewer pair. Start item N+1 only when item
   N is Done.
3. Split work into the smallest item that ships on its own (one clip, one
   post, one slide, one PR).
4. One owner per item: the owner implements it and self-reviews against the
   Definition of Done (DoD), written before work starts.
5. The reviewer runs an acceptance review against the same DoD and returns
   specific, checkable comments, or signs off.
6. Repeat the [PDCA loop](multi-agent-pdca.md#loop) until owner and
   reviewer both sign off explicitly.
7. Done = both sign-offs AND the item is shipped (published, scheduled, or
   merged). Signed-off work that has not shipped is still WIP.
8. Only after Done, pull the next item.
9. After every review round, log each lesson as failure → root cause →
   preventive rule (a check, template, or script change).
10. Apply that preventive rule to the next item before starting it. Each
    cycle should take fewer rounds and less time than the last; track rounds
    and time per item.
11. Fix the cause, not the instance. A repeated defect type gets an
    automated or checklist check that would have caught it.
12. Route lessons: generic design, editing, and process skills go to
    ChaosEngine; project-specific lessons go to the project's own repo or
    skills. The engineering advisor opens those PRs. A project lesson also
    updates that project's specialized instructions for token use, accuracy,
    or performance. A harness lesson is generalized (no paths, secrets,
    emails, or project names) and filed with the user's GitHub account only
    after one machine-local consent (`learning_session.py consent allow`).
    That question is asked once. `allow` stands until `consent deny`.
13. Batch engine updates: merge lesson PRs as they come, but reinstall or
    reload ChaosEngine once, at the end of the work batch.
14. Roles:
    - **Design owner** owns and implements all design work (UI, images,
      video, presentations) and self-reviews it.
    - **Acceptance reviewer** does the acceptance review.
    - **Engineering advisor** does only code and engine work; it may advise
      during planning and decisions, but never implements or accepts design
      work.
15. Blocked: flag it at once and swarm on the current item. Never start a
    new item to stay busy.

## Unstructured request

Rephrase an unstructured ask as a testable requirement, write the smallest
spec, then plan, do, check, and act. One writer. A same-model delegate only
when a measured receipt shows the child costs less than continuing: companion
shorthand shortens the handoff and does not remove the child's second context.

## Session close

Before ending, list every user ask from the session. Mark a finished ask `✓`
and a missed ask `✗` with the blocker beside it. Run one more plan-do-check-act
pass on every `✗`. Stop when each row is `✓`, or the blocker is proved and
written on that row.

## Per-iteration checklist

- [ ] Exactly one item in progress for this pair?
- [ ] DoD written before work started, and owner self-reviewed against it?
- [ ] Every reviewer comment specific and testable, or an explicit sign-off?
- [ ] Owner and reviewer each ran the [pre-done self-review](../skills/pre-done-self-review/SKILL.md):
      rules reconciled, every Blocked/Skipped row cites `rule:` + `alt:`,
      views match the table, `## What's missing` answered, and
      `tool.py pre-done <report.md>` exits 0?
- [ ] Both sign-offs, and the item shipped or scheduled?
- [ ] Each lesson logged as failure → root cause → preventive rule?
- [ ] Preventive rule applied to the next item's plan, template, or checks?
- [ ] Each lesson routed (generic → ChaosEngine, project → its own repo),
      with the PR handed to the engineering advisor?
- [ ] Rounds and cycle time recorded and shorter than last time? If not, why?
- [ ] No engine reinstall until the batch ends?

## Parallel streams (file-disjoint only)

WIP limit: 1 item still applies **inside** each owner–reviewer pair (one
stream). An orchestrator may run several streams only when their writable
paths do not overlap, after an ownership manifest and conflict check (peer
owned globs in the manifest, not only open PRs / dirty trees), under the
system cap and disk gate in [parallel streams](parallel-streams.md).
Same-file work stays serial or stacked. Do not start stream N+1 in a pair
until that pair's current item is Done. After a stream's item is Done
(merged), that stream runs
[post-merge own cleanup](parallel-streams.md#post-merge-own-cleanup) for its
own recorded worktree / branch / scratch only.

## Sources

1. Kanban Guide – limit WIP, manage flow, pull work:
   https://kanbanguides.org/english/
2. The Deming Institute – PDSA cycle: https://deming.org/explore/pdsa/
3. Toyota – Toyota Production System (Jidoka, Just-in-Time, kaizen):
   https://global.toyota/en/company/vision-and-philosophy/production-system/
4. Lean Enterprise Institute – continuous (one-piece) flow:
   https://www.lean.org/lexicon-terms/continuous-flow/
5. Scrum Guide – Definition of Done, Retrospective:
   https://scrumguides.org/scrum-guide.html
6. Reinertsen, *The Principles of Product Development Flow* (small batches,
   WIP constraints):
   https://www.goodreads.com/book/show/6278270-the-principles-of-product-development-flow
