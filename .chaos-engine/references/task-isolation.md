# Task isolation

For a new task, never treat the process working directory as planning authority
merely because the session started there. Before task-specific planning or
discovery, locate the verified primary checkout and require clean, unlocked,
exclusive state; fetch and prune the configured upstream, verify its configured
default branch, then fast-forward the local default branch to the immutable
upstream tip. SessionStart performs that gate once per session_id: when the
primary checkout is already on the configured default branch, it may discard
uncommitted files there and reset to the upstream tip; when a leftover task
branch is dirty, it halts and preserves that work. Unique commits are never
discarded. Concurrently owned, locked, divergent, unfetched, or unverifiable
state still fails closed for mutation and fail-open as an advisory.

After this gate, freeze the base commit, create a dedicated `ChaosEngine/*`
branch and linked worktree rooted at it, and perform planning, discovery, and
implementation there. SessionStart creates or reuses one sibling detached
session worktree named from the host session_id; the agent then creates the
`ChaosEngine/*` branch inside it. Stop never deletes that worktree. SessionEnd
removes it only after merge is recorded and the tree is clean, keeping the
local branch. Ordinary tasks launch no background store processes and
do not refresh or validate every knowledge store; the configured maintenance
owner remains responsible for derived-store updates.

An explicit continuation of a named local or remote task branch is the sole
fresh-default exception. Fetch when a remote exists, verify the named branch
and its intended base, preserve its work, and isolate the continuation in a
dedicated worktree; never silently restart it from the default branch. Resume
or compact of the same session_id reuses the existing session worktree.

Resolve repository identity, upstream, default branch, primary-checkout path,
and store commands from the selected profile, adapters, configuration, or
integration playbooks. Never encode one repository or machine as portable
policy.
