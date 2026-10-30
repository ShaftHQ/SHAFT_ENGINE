# Commit identity (ChaosEngine author scope)

## Rule

Commit as **ChaosEngine** `<chaosengine@mail.grokbot.com>` **only** in the two
remotes returned by `allowed_remotes()` in
[`commit_identity.py`](../commit_identity.py): the public engine GitHub
repository and its companion user-guide pages repository. Every other
repository uses the human owner's own git identity (name + email). Never set
`GIT_AUTHOR_*` / `user.name` / `user.email` to ChaosEngine outside those two
remotes.

## Why

A bot author on third-party or personal forks breaks CLA checks, confuses
attribution, and can look like spoofing. Scoped authorship keeps engine and
user-guide automation clear without leaking the bot identity elsewhere.

## Check

`git remote get-url origin` matches one of `allowed_remotes()` before any
commit that sets ChaosEngine as author. Worker briefs inherit this default.
`assert_chaosengine_author_allowed(project)` in
`chaos-engine/commit_identity.py` refuses ChaosEngine authorship on any
other remote.

## Follow-up

[bot entry](bot-entry.md), [worker keep-alive](worker-keep-alive.md),
[`worker_ce_entry.py`](../skills/local-agency/scripts/worker_ce_entry.py).
