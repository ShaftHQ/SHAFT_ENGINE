# Orchestrator bootstrap

Use once the entrypoint's solo-or-orchestrate rule selects the orchestrated
mode. Counting the owner's unrelated asks in flight is enough to decide that,
and needs nothing from this file; step 2 then gathers the rest. This phase
gathers evidence and dispatches, and main thread does no task work while it
does. A single stream skips this file and is worked solo, in sequence.

1. Establish repositories in scope. Default current repo; add companion docs
   only when behavior requires it.
2. Gather live issues, PRs, worktrees, recent default-branch CI, native
   Memory, MemPalace, and Graphify. Verify candidate files live.
3. Queue in-progress work, open PRs, shipped bugs/red default branch, direct
   owner asks, then backlog. Finish before opening new fronts.
4. Compare file scopes. Sequence overlap; isolate independent writers in
   worktrees, within the concurrency cap in [delegation](delegation.md).
   Default one writer at a time; parallel only on owner request.
5. After loading every assigned ticket, group related work into the fewest PRs
   that still keep one problem per issue. Ticket substantial multi-part work
   before dispatch: tracker plus linked subtask issues.
6. Create a delivery loop whose done condition is every in-scope ticket
   delivered or explicitly out of scope. Do not stop after a plan, a status
   table, or one PR.
7. Dispatch bounded specs: exact behavior, files, precedent, exclusions, and
   deferred consolidated proof command. Select the most intelligent, default, or
   mechanical capability using [delegation](delegation.md), never provider
   identity. After every dispatch, update the live status table.
8. Stay available for architecture and consult decisions, on the check-in
   cadence in [delegation](delegation.md).
9. Review the actual diff and tests as [delegation](delegation.md) defines.
   Main thread owns synthesis and final verification. Collect the writer's
   bounded learning handoff, then destroy the finished writer and continue.
10. After every delivery completes, run the one root Learning Session immediately before the final report.

For branch, tracker, and PR mechanics use
[work GitHub](work-github-playbook.md).
