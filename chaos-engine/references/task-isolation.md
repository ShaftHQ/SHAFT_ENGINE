# Task isolation

For a new task, never treat the process working directory as planning authority
merely because the session started there. Before task-specific planning or
discovery, locate the verified primary checkout and require clean, unlocked,
exclusive state; fetch and prune the configured upstream, verify its configured
default branch, then fast-forward the local default branch to the immutable
upstream tip. Stop on dirty, concurrently owned, locked, divergent, unfetched,
or unverifiable state.

After this gate, freeze the base commit, create a dedicated `ChaosEngine/*`
branch and linked worktree rooted at it, and perform planning, discovery, and
implementation there. Ordinary tasks launch no background store processes and
do not refresh or validate every knowledge store; the configured maintenance
owner remains responsible for derived-store updates.

An explicit continuation of a named local or remote task branch is the sole
fresh-default exception. Fetch when a remote exists, verify the named branch
and its intended base, preserve its work, and isolate the continuation in a
dedicated worktree; never silently restart it from the default branch.

Resolve repository identity, upstream, default branch, primary-checkout path,
and store commands from the selected profile, adapters, configuration, or
integration playbooks. Never encode one repository or machine as portable
policy.
