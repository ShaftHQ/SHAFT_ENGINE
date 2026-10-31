# PR scope check (H-SCOPE)

## Rule

Before merge (and before claiming delivery), the PR **diff paths** must match the
**stated scope** in the PR body / plan. Out-of-scope paths → **stop**.

Declare scope with either:

```markdown
<!-- chaos-engine:scope chaos-engine/,scripts/ci/,tests/scripts/ -->
```

or:

```markdown
## Scope
- chaos-engine/
- scripts/ci/
- tests/scripts/
```

## Why

PRs that claim a narrow gate but ship unrelated tree churn hide regressions and
defeat review. Scope is a contract with the reviewer.

## Check

```bash
git diff --name-only origin/main...HEAD > /tmp/changed.txt
python scripts/ci/pr_scope_check.py --body /tmp/pr-body.md --changed /tmp/changed.txt
```

Missing scope declaration → FAIL. Empty diff → PASS when scope is declared.

## Pre-merge checklist

- [ ] PR body has `## Scope` or `<!-- chaos-engine:scope ... -->`
- [ ] `pr_scope_check.py` PASS on `git diff --name-only base...HEAD`
- [ ] Out-of-scope findings fixed (trim diff or widen stated scope deliberately)

## See also

- `overlay_pre_push.py` (push gates)
- `commit-identity.md` (H-IDENTITY)
