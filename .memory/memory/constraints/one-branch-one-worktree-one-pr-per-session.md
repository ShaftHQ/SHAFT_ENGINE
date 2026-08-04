Branch and worktree shape for a session. The PR-count default is deliberately NOT recorded
here: it lives in `.agents/skills/act-as-mohab/references/work-github-playbook.md` Sec. 3b
(tracking issue, one real issue per subtask, and one PR per group of related subtasks), and
that section explicitly supersedes the older framing this object used to carry. Read it
there; a second copy in memory is what let two shapes both look current.

What survives from the 2026-07-17 directive and is still true: do a session's work in one
worktree on one branch, and do not spin up a new branch or worktree per task, issue, or
phase within a session. If a branch from earlier in the session is already open, keep using
it rather than opening a second one. This does not override the worktree-housekeeping
cleanup rules for stale leftover worktrees from past sessions -- it is about how many
branches and worktrees a session actively creates going forward.

EXCEPTION (user directive, 2026-07-17, #3643 session): when a batch of sub-issues has
file-level dependencies between them (a later item needs a file or API that a not-yet-merged
earlier item changed), grouping them is the wrong shape -- it is exactly what invites
parallel isolation:"worktree" dispatch against a stale base (see
[[agent-tool-isolation-worktree-branches-from-session-start-head-not-the-evolving-session-branch]]).
In that specific situation the user directed: merge the currently-completed, non-conflicting
work first (PR, wait green, merge to origin/main), then for each remaining dependent item cut
a FRESH branch off the just-updated origin/main, implement that one item alone, PR it, wait
for green CI, merge it, then cut the next fresh branch off the new origin/main for the
following item -- one branch, PR and merge per item, sequential, never two file-dependent
items in parallel worktrees. Treat this exception as standing guidance for any session with
file-dependent sub-issues, not a one-off.
