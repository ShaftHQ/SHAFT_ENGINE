Owner directive (2026-07-18): a delegated agent or background job must not run unattended
indefinitely. The check-in interval itself is deliberately not recorded here --
`.agents/skills/act-as-mohab/references/delegation.md` owns the single figure, and a second
number in memory is exactly how two different intervals both came to look current. Read it
there.

What this object carries that guidance does not is the ladder past the first check-in. On
each interval, do something substantive: ask for a status snapshot, or inspect the partial
output and the worktree -- never a heartbeat. After three consecutive check-ins with no
convergence, stop waiting and intervene: narrow the scope, split the task, or stop and
respawn. CI monitors streaming per-check events are exempt; they have their own cadence, and
the rule targets silence, not duration. Rationale: a long-running Allure agent ran silent for
over an hour in the originating session and the owner flagged it.
