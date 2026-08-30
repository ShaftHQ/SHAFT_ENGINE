---
name: omniroot
description: >-
  Optional local OmniRoute transport for bounded delegated implementation.
  Use after selecting an orchestrated execution workflow and before dispatching
  an external local process.
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

`READY` requires loopback health, endpoint-key presence without reading or
recording its value, and an unexpired user-local attestation. Qualification is
cached only for the root session and invalidated when route, server build, or
attestation changes. Dispatch uses isolated existing worktrees, a minimal
environment, argument-list process invocation, private atomic manifests, and
terminal receipts. Root verifies all returned claims and imports each delegate
learning disposition before the sole Learning Session.

The operator configures the user-local configuration and attestation outside
the repository. The runner treats both as untrusted input and fails closed.
