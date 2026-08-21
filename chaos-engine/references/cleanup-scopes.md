# Cleanup scopes

Cleanup policy is portable. Repository identities, filesystem locations,
default branches, users, hosts, agents, and services come only from the user,
selected profile, adapter, configuration, or integration playbook. Never turn
one execution's facts into canonical defaults.

## Common preflight

Resolve the configured upstream and inventory dirty files, local branches,
worktrees, active operations, locks, stale administrative records, and unique
commits before mutation. Preserve unique commits unless their discard is
explicitly authorized. Report mutations, preserved state, and verification.
Never rewrite remote history as cleanup.

### Task scope (default)

Freeze the pre-task baseline. Maintain an append-only ownership manifest during
the task; once an artifact is recorded, its ownership record is immutable.
Safe cleanup of task-owned artifacts is the default and needs no broader
repository or machine cleanup grant. Clean only artifacts the task created and recorded in that manifest, in
dependency order, and verify each is gone or intentionally retained. A
pre-existing artifact stays outside deletion scope even if the task touches it. Preserve and report
pre-existing, unknown, dirty, locked, or concurrently owned state. Residue in
the same repository does not widen the scope.

### Repository scope (explicit)

Repository-wide cleanup is never inferred from task cleanup. An explicit
request may widen cleanup to the identified repository. Complete
the common preflight, normalize only that repository, and require one clean
expected checkout at the configured upstream tip. Use its verified primary
checkout. Refresh and validate all three knowledge stores: native Memory,
Graphify, and MemPalace. Do not touch sibling repositories or machine-wide
caches. Preserve and halt on pre-existing unknown, dirty, locked, or
concurrently owned state unless its discard is separately authorized.

### Machine scope (approval-gated)

Machine cleanup is never inferred from either narrower scope. This scope
requires specific user approval because it crosses repository or
workspace boundaries. Build an exact validated manifest of approved targets
and survivors before deletion. Resolve each target again immediately before
mutation; require containment within approved roots, reject reparse points and
identity changes, and halt on live locks or ambiguous ownership. Process only
manifest entries. Normalize each survivor's checkout. Refresh and validate all
three knowledge stores: native Memory,
Graphify, and MemPalace. Then run a second inventory. Approval for one target
class never authorizes another.

## Verification helper

The repository hygiene reporter is read-only by default. Its opt-in verification mode
fails unless the repository has one clean expected worktree and a local branch
whose name matches the configured upstream branch at its tip, with no active
Git operation or stale/prunable administrative state. An explicit override or
one unambiguous configured remote HEAD supplies that trust anchor; the current
branch never authenticates itself. Reporting a violation never authorizes its
deletion.

### Bounded task cleanup

For minor mechanical cleanup, `scripts/ci/worktree_hygiene.py` accepts one
explicit `--cleanup-task-worktree` target. The caller must also supply an exact
owned branch, expected HEAD, expected worktree-diff SHA-256, and a schema-v1
task ownership manifest containing the same four values. The action rechecks
clean status, lock/current/main/prunable state, active Git operations, merged
pull-request state, upstream ancestry, branch HEAD, and diff immediately before
using non-force `git worktree remove`.

Keep the ownership manifest outside the target worktree so cleanup cannot erase
its own evidence. The action never deletes the local branch because another
worktree can attach that same ref concurrently after target removal. It reports
the retained branch for separate cleanup after fresh ownership, attachment,
ancestry, and pull-request checks.

The action never performs repository- or machine-scope cleanup, never removes
remote branches, and rejects `origin` and `gh-pages`. A missing, ambiguous, or
changed proof blocks the action; it does not widen or guess cleanup authority.
Invocation from the target worktree or any descendant also blocks cleanup,
independent of which checkout was supplied as `--root`.
