# Commit identity (H-IDENTITY)

## Rule

ChaosEngine `<chaosengine@mail.grokbot.com>` may author commits **only** in the
two remotes returned by `allowed_remotes()` (public engine repo + its
user-guide pages repo). Every other remote must use the human **owner** git
identity (`user.name` / `user.email`). Pre-commit and PreToolUse **block** the
commit when ChaosEngine authorship would land outside that map (the consumer
repo miss class that produced six ChaosEngine-authored commits).

## Why

Consumer / personal / client repos that inherit a ChaosEngine `user.email`
pollute history and break attribution. The allow-list is the public engine +
its user-guide pages only.

## Check

```python
from commit_identity import (
    allowed_remotes,
    repo_identity_map,
    pre_commit_identity_failures,
    assert_chaosengine_author_allowed,
)
assert len(allowed_remotes()) == 2
# Pre-commit / `python commit_identity.py --pre-commit` exits 1 on failures:
assert pre_commit_identity_failures(
    name="ChaosEngine",
    email="chaosengine@mail.grokbot.com",
    remote_url="https://github.com/example/designer-workspace.git",
)
```

Git hook (`git_hooks.py` pre-commit) runs `commit_identity.py --pre-commit`
after the secret guard. Guard PreToolUse calls
`commit_identity_pre_commit_reason` on `git commit`.

## Owner identity

On any non-allow-listed remote, set the owner's name and email before
committing. ChaosEngine must not remain as the local `user.email` in those
clones.

## See also

- `bot-entry.md` / `user-guide.md` (identity + push path)
- `git_hooks.py` (pre-commit wiring)
