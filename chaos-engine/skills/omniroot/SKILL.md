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

Use only the standard-library runner:

```text
python3 chaos-engine/skills/omniroot/scripts/runner.py probe
python3 chaos-engine/skills/omniroot/scripts/runner.py dispatch ...
python3 chaos-engine/skills/omniroot/scripts/runner.py status ...
python3 chaos-engine/skills/omniroot/scripts/runner.py cancel ...
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

Qualification is cached only for the root session and invalidated when route,
server build, launcher, or attestation changes. Dispatch requires a clean real
Git worktree, non-overlapping non-private path ownership, argument-list process
invocation, a minimal environment, a bounded runtime, and atomic mode-`0600`
state. Standard output and error are drained without an unbounded buffer,
secret-shaped values are redacted, and each retained stream is capped at 16
KiB in a private diagnostic artifact. A timeout terminates the isolated process
group; unsupported or unprovable termination fails closed. Its manifest
freezes task/workflow/root/base/integration/qualification/delegate/process/
cadence/deadline/timeout/HEAD/diagnostic/receipt facts; its terminal receipt freezes outcome,
exit, clean state, changed paths, checks, blockers, adjacent findings, and
learning disposition plus the diagnostic hash and truncation/timeout flags.
Receipt creation rejects an exit code that conflicts with captured process
evidence. Unsupported cancellation or stale process identity
quarantines state. Root verifies all returned claims and imports each delegate
learning disposition before the sole Learning Session.

The operator configures the user-local configuration and attestation outside
the repository. The runner treats both as untrusted input and fails closed.
