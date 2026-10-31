# Commit identity (H-IDENTITY)

## Rule

ChaosEngine `<chaosengine@mail.grokbot.com>` may author commits **only** in the
two remotes returned by `allowed_remotes()` — the public **engine** repo and its
**user-guide pages** repo (exactly those two `Owner/Name` strings; print them
with `python3 -c "from commit_identity import allowed_remotes; print(allowed_remotes())"`
or read the origin-only SPEC). Every other remote must use the human **owner**
git identity (`user.name` / `user.email`). Pre-commit and PreToolUse **block**
the commit when ChaosEngine authorship would land outside that map (the
consumer repo miss class that produced six ChaosEngine-authored commits).

## Why

Consumer / personal / client repos that inherit a ChaosEngine `user.email`
pollute history and break attribution. The allow-list is the public engine +
its user-guide pages only (`allowed_remotes()`).

## Check

```python
from commit_identity import (
    allowed_remotes,
    repo_identity_map,
    pre_commit_identity_failures,
    assert_chaosengine_author_allowed,
)
assert len(allowed_remotes()) == 2
# Pre-commit / `commit_identity.py --pre-commit` exits 1 on failures:
assert pre_commit_identity_failures(
    name="ChaosEngine",
    email="chaosengine@mail.grokbot.com",
    remote_url="https://github.com/example/designer-workspace.git",
)
```

Git hook ([`git_hooks.py`](../git_hooks.py) pre-commit) runs [`commit_identity.py`](../commit_identity.py) `--pre-commit`
after the secret guard. Guard PreToolUse calls
`commit_identity_pre_commit_reason` on `git commit`.

## Owner identity

On any non-allow-listed remote, set the owner's name and email before
committing. ChaosEngine must not remain as the local `user.email` in those
clones.

When the host configures an expected owner (local, **untracked** — never commit
personal name/email into the portable tree):

- Env: `CE_OWNER_IDENTITY="Name <email@example.com>"` (or `CE_OWNER_NAME` +
  `CE_OWNER_EMAIL`)
- Or file: `.chaos-engine-state/owner-identity` (one line, same shape; path is
  gitignored)

…then non-ChaosEngine commits on non-allow-listed remotes must match that
configured identity (email required; name when provided). When unset, any
non-ChaosEngine identity still passes (portable default). Org bot charters may
point operators at the env/file without putting personal addresses in-repo.

## Entry / doctor warning

`entry_identity_warning(project)` emits one line when the **repo-local**
`user.name` / `user.email` is ChaosEngine outside `allowed_remotes()`. Doctor
and `tool.py entry` surface it before more mis-attributed commits land.

## See also

- `bot-entry.md` / `user-guide.md` (identity + push path)
- `git_hooks.py` (pre-commit wiring)
- Origin-only names of both remotes: [guides/ce-retro-2026-10-31-6-SPEC.md](../guides/ce-retro-2026-10-31-6-SPEC.md) (H-IDENTITY)
