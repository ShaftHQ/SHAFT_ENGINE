# OmniRoute runner

Moved from [OmniRoute](../SKILL.md) (#6176) so the skill body stays under its cap.

Use only the standard-library [runner](../scripts/runner.py) (proof helpers in [proof](../scripts/proof.py)):

```text
python3 .chaos-engine/skills/omniroute/scripts/runner.py probe
python3 .chaos-engine/skills/omniroute/scripts/runner.py candidates --capability mechanical|default|most-intelligent
python3 .chaos-engine/skills/omniroute/scripts/runner.py candidates --capability default --task coding
python3 .chaos-engine/skills/omniroute/scripts/runner.py proof --required --receipt <private-state>/receipt.json
python3 .chaos-engine/skills/omniroute/scripts/runner.py dispatch --contract <private-state>/dispatch.json
python3 .chaos-engine/skills/omniroute/scripts/runner.py status ...
python3 .chaos-engine/skills/omniroute/scripts/runner.py cancel ...
python3 .chaos-engine/skills/omniroute/scripts/runner.py complete --contract <private-state>/complete.json
```

The only automatic endpoint is `http://127.0.0.1:20128/`. The runner permits
no redirect or remote override. It emits only these readiness states:
`ABSENT`, `UNHEALTHY`, `UNAUTHENTICATED`, `ROUTE_UNQUALIFIED`, `READY`, and
`RUNTIME_EXHAUSTED`.

`READY` means the loopback API answers and the live catalog has at least one
model with remaining tokens. Then use it. Catalog queries use the local CLI
session and must not inherit ambient `OMNIROUTE_API_KEY` or `OMNIROUTE_BASE_URL`
values that return "No models found". Missing operator config does not block
READY. Dispatch launches `omniroute run --model --provider <target>` from the live
catalog (`claude`, then `opencode`, then `codex` when those binaries exist).
The runner never reads, prints, or records keys, routes, targets, or
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
files. OmniRoute never reads, passes, prints, or stores endpoint keys or CLI
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
health disables ambient proxies and rejects redirects. Dispatch binds one absolute protected executable (device/inode/owner/mode/size/mtime/SHA-256) and revalidates before execution. Dispatch requires distinct clean linked delegate and integration worktrees (no untracked files, atomic ownership reservation, path-overlap rejection, argv invocation, minimal env, bounded runtime, private state rejecting symlinks/unsafe ownership). Stdout/stderr drain without unbounded buffers; secret-shaped values redacted; each retained stream capped at 16 KiB in a private diagnostic. Timeout/cancel: `SIGTERM`, then `SIGKILL`, prove process-group death before releasing state. Unsupported durable process identity fails closed before native delegation. Its manifest
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

