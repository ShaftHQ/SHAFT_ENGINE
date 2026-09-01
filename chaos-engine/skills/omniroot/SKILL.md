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

Do not install OmniRoute, create provider accounts, or write operator
credentials. Do not expose credentials, account data, prompts, or consumer
code outside the approved bounded task. Receipts and repository files never
persist route, model, or provider IDs; live stdout of `candidates` may name
them for the current dispatch only.

## Ensure the local gateway

Dashboard: `http://127.0.0.1:20128/home`. Health (anonymous JSON
`status`/`timestamp` is enough here):

```text
command -v omniroute
curl -sf --max-time 2 http://127.0.0.1:20128/api/health
```

If `omniroute` is missing, do not install it; use native host-session models.
If the binary exists and health fails, start loopback only:

```text
OMNIROUTE_SERVER_HOST=127.0.0.1 omniroute serve --port 20128 --no-open
```

Never bind a non-loopback address. Never use a remote `--base-url`.

## Live catalog on every dispatch

Do not read cache files. Do not keep a session catalog file. Remaining tokens
change after each delegate, so query both of these immediately before every
dispatch:

```text
omniroute --output json models
omniroute --output json usage quota
python3 chaos-engine/skills/omniroot/scripts/runner.py candidates --capability mechanical|default|most-intelligent
```

Do not add `--json` after the `models` subcommand: that form prints a table, not JSON. Use `--output json` before `models`.
Do not use `omniroute openapi try /api/models/catalog` without the CLI session;
it returns HTTP 401.

### Decode

1. Strip ANSI with `\\x1b\\[[0-9;]*[A-Za-z]`.
2. Find the first `{` or `[`.
3. Parse with `json.JSONDecoder().raw_decode` so trailing extra JSON is ignored.
4. Catalog is a JSON array of objects with `id` and `provider`.
5. Quota is a JSON array of objects with `provider`, `remaining`, and `state`.
6. Join on alphanumeric-lowercased provider ids (`glm-cn` matches `glmcn`).
7. Drop `state == "exhausted"` or `remaining <= 0`.

### Rank (dynamic, from the live ids)

Classify each remaining `id` by its own tokens, not a stored model list:
`low|lite|flash|air|mini|nano|turbo|haiku|small` = mechanical;
`high|max|pro|ultra|opus|thinking|reasoner` = most-intelligent;
otherwise default. Architecture, review, and analytical work use only
most-intelligent. Implementation uses `default` first, then most-intelligent,
then mechanical. Do not pin a Codex profile model such as Gemini Flash-Lite.
Empty result is `RUNTIME_EXHAUSTED`.

Retry is chosen from the failure, not from a pinned profile:

- HTTP 429 / rate-limit / resource_exhausted: do not retry the same identity.
  Requery the catalog, skip that `identitySha256`, pick the next remaining
  model, and relaunch Codex with `--model` / `--provider`.
- Timeout or a single network blip: retry the same catalog pick once.
- HTTP 401/403 or invalid key: stop. Do not retry. Fix the endpoint credential.
- Empty remaining catalog: `RUNTIME_EXHAUSTED`, then native host models.

Never pin a model in a Codex profile. Fetch the live catalog, rank for the
task, then launch `omniroute run --model '<id>' --provider '<provider>' codex`.

### Dispatch and follow-through

```text
omniroute run --model '<id>' --provider '<provider>' codex -- exec --ephemeral --approve-for-me -C '<worktree>' '<prompt>'
```

Then follow [orchestrator follow-through](../../references/orchestrator-follow-through.md)
until the delegate exits with closing notes. Rank free/remaining catalog
entries first. If those fail, use any other model the local endpoint can call.
Native host models only when OmniRoute itself cannot run.

Canonical orchestration must probe the fixed loopback endpoint before native
fallback, with no endpoint prompt. On `READY` after a live `candidates` pick,
dispatch through `omniroute run` as above. A concrete `RUNTIME_EXHAUSTED`
health result, empty remaining catalog, or sealed-launcher exit code `78`
permits native implementer fallback.

## Runner

Use only the standard-library [runner](scripts/runner.py):

```text
python3 chaos-engine/skills/omniroot/scripts/runner.py probe
python3 chaos-engine/skills/omniroot/scripts/runner.py candidates --capability mechanical|default|most-intelligent
python3 chaos-engine/skills/omniroot/scripts/runner.py dispatch --contract <private-state>/dispatch.json
python3 chaos-engine/skills/omniroot/scripts/runner.py status ...
python3 chaos-engine/skills/omniroot/scripts/runner.py cancel ...
python3 chaos-engine/skills/omniroot/scripts/runner.py complete --contract <private-state>/complete.json
```

The only automatic endpoint is `http://127.0.0.1:20128/`. The runner permits
no redirect or remote override. It emits only these readiness states:
`ABSENT`, `UNHEALTHY`, `UNAUTHENTICATED`, `ROUTE_UNQUALIFIED`, `READY`, and
`RUNTIME_EXHAUSTED`.

`READY` means the loopback API answers and the live catalog has at least one
model with remaining tokens. Then use it. Catalog queries use the local CLI
session and must not inherit ambient `OMNIROUTE_API_KEY` or `OMNIROUTE_BASE_URL`
values that return "No models found". Missing operator config does not block
READY. Dispatch launches `omniroute run --model --provider` from the live
catalog. The runner never reads, prints, or records keys, routes, targets, or
assignments.

OmniRoute 3.8.50 may return only `status` and `timestamp` to an anonymous
`/api/health` request. That is never build evidence. For that exact response
shape only, the runner may use an owner-verified local OmniRoute CLI against
the same fixed loopback endpoint with a temporary working directory and
scrubbed ambient environment, retaining only a healthy semantic-version
`build` or `version`. The runner verifies every executable's owner, private
group, non-public ancestry, descriptor identity, and pre/post-exec identity;
the CLI response is hard-bounded. The child receives an isolated temporary
`HOME`, data, and XDG directories, so it cannot migrate or alter operator
files. OmniRoot never reads, passes, prints, or stores endpoint keys or CLI
token material; the verified CLI resolves its local machine-token proof.
Missing, malformed, untrusted, changed, oversized, timed-out, unhealthy, or
non-versioned CLI evidence remains `UNHEALTHY`.

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

Optional user-local launcher config lives outside the repository. A missing
file is not a failure. Unsafe files are skipped in favor of the PATH launcher.

## Delegate continuity

Dispatch may opt into bounded continuity. Omit `continuity` for unchanged
legacy behavior. Continuity freezes capability floor, maximum attempts,
retryable exit codes, bounded backoff, authority/checkpoint hashes, completed
action hashes, tracker/PR hashes, and ordered alternate identity/session hashes.
At most four writers may participate: one initial writer plus no more than three
alternates, with no more than four total attempts. Each private alternate also
carries one validated target and bounded argument list. The supervisor keeps the
sealed launcher fixed while selecting those inputs in memory for each attempt.
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

For opted-in dispatches, runner starts its private `_supervise` process instead
of one-shot `_capture`. Supervisor retains raw alternate session identifiers
only in its inherited process environment, removes them before launching any
delegate, and never writes them to disk. It observes sealed-launcher exit,
proves process-group death, applies backoff and capability selection, registers
replacement, then launches candidate-specific private inputs against the same
frozen task and authority. Final successful evidence
moves normal `status` flow to review without owner input. `_supervise` is an
internal runner command, not an operator-facing interface.
The original timezone-aware deadline bounds all attempts, backoff, and process
runtime. Expiry blocks continuity before another launch.
