---
name: omniroot
description: >-
  Use when an orchestrated workflow may dispatch bounded implementation through
  an optional local OmniRoute process.
license: MIT
---

# OmniRoot

Optional provider-neutral transport. It is not a workflow owner; select the
canonical workflow in [execution workflows](../../references/execution-workflows.md)
first. Missing, stopped, unauthenticated, exhausted, or unqualified OmniRoute
is normal: use a qualified native implementer or `SOLO`.

Do not install, start, configure, authenticate, or choose routes for OmniRoute.
Do not expose credentials, route internals, account data, prompts, or consumer
code outside the approved bounded task.

## Runner

Use only the standard-library [runner](scripts/runner.py):

```text
python3 chaos-engine/skills/omniroot/scripts/runner.py probe
python3 chaos-engine/skills/omniroot/scripts/runner.py dispatch --contract <private-state>/dispatch.json
python3 chaos-engine/skills/omniroot/scripts/runner.py status ...
python3 chaos-engine/skills/omniroot/scripts/runner.py cancel ...
python3 chaos-engine/skills/omniroot/scripts/runner.py complete --contract <private-state>/complete.json
```

The only automatic endpoint is `http://127.0.0.1:20128/`. The runner permits
no redirect or remote override. It emits only these readiness states:
`ABSENT`, `UNHEALTHY`, `UNAUTHENTICATED`, `ROUTE_UNQUALIFIED`, `READY`, and
`RUNTIME_EXHAUSTED`.

`READY` requires loopback health, an executable operator-owned launcher, and
an unexpired user-local attestation. The launcher may load its own restricted
credential; direct environment-key mode remains supported, without reading or
recording the key. Attestation confirms route-policy and key-identity hashes,
a hash and known-existing confirmation for a denied probe target, denied-probe
enforcement, terms,
privacy, no-cost, and no-paid-fallback conditions. It never writes routes,
targets, credentials, launcher arguments, or assignments to state.

The user-local launcher configuration accepts `invocationMode: "gateway"` or
`"direct"`; the default is `gateway` for compatibility. Gateway mode invokes
the launcher with the target, fixed loopback port, credential-environment flag,
and `--` before delegate arguments. Direct mode passes only the configured
launcher argv followed by delegate arguments, for protected launchers that own
their endpoint and profile. The mode is validated and included only in the
qualification hash; manifests never record route or model names.

Qualification is freshly probed before every dispatch; volatile health and
authentication facts never come from a stale `READY` cache. Operator config is
a regular owner-owned private file (mode `0600` where permission bits exist).
Dispatch reads one no-follow config descriptor, seals the verified launcher
into owner-private state, and executes that immutable copy. Loopback
health disables ambient proxies and rejects redirects. Dispatch resolves one
absolute protected executable, binds device, inode, owner, mode, size, mtime,
and SHA-256 content, then revalidates it immediately before execution. Dispatch
requires distinct clean linked delegate and integration Git worktrees from the
expected repository, including
no untracked files, interprocess-atomic ownership reservation, ancestor/descendant path
overlap rejection, argument-list process invocation, a minimal environment, a
bounded runtime, and private state whose components reject symlinks and unsafe
ownership or permissions. Standard output and error are drained without an unbounded buffer,
secret-shaped values are redacted, and each retained stream is capped at 16
KiB in a private diagnostic artifact. Redaction removes exact known credential
values before persistence plus credential-shaped patterns. A timeout or cancel
waits after `SIGTERM`, sends `SIGKILL` to survivors, and proves process-group
death before releasing state. Unsupported durable process identity or process-tree
termination fails closed before state mutation to native delegation; OmniRoot transport is not claimed
on that platform. Its manifest
freezes task/workflow/root/base/integration/qualification/delegate/process/
cadence/deadline/timeout/HEAD/diagnostic/receipt facts; its terminal receipt freezes outcome,
exit, clean state, changed paths, checks, blockers, adjacent findings, and
learning disposition plus the diagnostic hash and truncation/timeout flags.
Runtime state defaults to the user's platform state directory, outside the
repository; explicit state paths inside managed worktrees are rejected.
Dispatch and completion each consume one owner-owned `0600` JSON contract,
covering workflow, root/task identity, ownership, integration target, cadence,
deadline, timeout, learning-runtime identity, and terminal evidence. The root
creates the learning runtime first; dispatch atomically registers the delegate
before launch and fails closed if registration cannot be proven. Corrupt or unsafe live manifests abort
reservation. Completion requires an existing manifest already in review,
blocked, or cancelled state; captured terminal diagnostics; ownership-bound
changed paths exactly matching the frozen-base-to-submitted-HEAD Git diff;
verified ancestry, real files, ownership, and clean Git HEAD; then creates one
fsynced atomic non-replaceable
private receipt. Receipt creation rejects an exit code that conflicts with captured process
evidence. Unsupported cancellation or stale process identity
quarantines state. Monitor and delegate PID, process identity, and process group
are tracked separately. Review and cancellation require proven delegate-group
death; surviving or unverifiable groups quarantine the run. Root verifies all returned claims and imports each delegate
learning disposition before the sole Learning Session.

The operator configures the user-local configuration and attestation outside
the repository. The runner treats both as untrusted input and fails closed.

## Delegate continuity

Dispatch may opt into bounded continuity. Omit `continuity` for unchanged
legacy behavior. Continuity freezes capability floor, maximum attempts,
retryable exit codes, bounded backoff, authority/checkpoint hashes, completed
action hashes, tracker/PR hashes, and ordered alternate identity/session hashes.
Raw prompts, credentials, links, provider/model names, commands, and local paths
never enter continuity state.

Replacement starts only after prior process-group death is proven. Lower
capability alternates are skipped. Learning registration precedes launch;
registration failure creates no participant or process. One live replacement
sets `replacement_running`, making repeated resume calls idempotent. Exhausted
attempts open the breaker and block; unverifiable process death or identity
quarantines. Terminal receipts include only redacted continuity hashes,
attempt/state, and participant hashes. Root still owns final evidence import and
the sole Learning Session.
