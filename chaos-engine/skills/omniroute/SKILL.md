---
name: omniroute
description: >-
  Use when an orchestrated workflow may dispatch bounded implementation through
  an optional local OmniRoute process.
license: MIT
---

# OmniRoute

Optional provider-neutral transport. It is not a workflow owner; select the
canonical workflow in [execution workflows](../../references/execution-workflows.md)
first. Missing, stopped, unauthenticated, exhausted, or unqualified OmniRoute
is normal: use a qualified native implementer or `SOLO`.

Do not install OmniRoute, create provider accounts, or write operator
credentials. Do not expose credentials, account data, prompts, or consumer
code outside the approved bounded task. Receipts and repository files never
persist route, model, or provider IDs; live stdout of `candidates` may name
them for the current dispatch only.

Operator guide: [guides/omniroute.md](../../guides/omniroute.md). Docs study:
[references/docs-study-notes.md](references/docs-study-notes.md). Living
lessons: [references/living-lessons.md](references/living-lessons.md).

## Agent machine vs user machine

`127.0.0.1:20128` is **host-local** to wherever `omniroute serve` runs (often
the user ROG). An agent box loopback is a different host and cannot see the
user dashboard. When the adopter asked for OmniRoute and Shell is not on that
user machine, say so, require Local Execution / the correct user `machineId`,
and stop. Do not invent remote `--base-url`. If OmniRoute is missing on the
user machine, guide install/serve from the operator guide — never install or
create provider accounts for the operator.

## Proof of dispatch when required

When the adopter/process-owner opts into OmniRoute, absence of an
`omniroute run` receipt (or a coding completion call_log) is a **failed
attempt / explicit blocker**. Catalog, `candidates`, health, and
CredentialHealth / `connection-test` probes alone are not progress. Normative
rules and CLI: [references/proof-of-dispatch.md](references/proof-of-dispatch.md).

```text
python3 chaos-engine/skills/omniroute/scripts/runner.py proof --required \
  --receipt <private-receipt.json>
```

## Ensure the local gateway

Dashboard: `http://127.0.0.1:20128/home` (on the serve host). Health (anonymous
JSON `status`/`timestamp` is enough here):

```text
command -v omniroute
curl -sf --max-time 2 http://127.0.0.1:20128/api/health
```

If `omniroute` is missing, do not install it; guide the operator or use native
host-session models when OmniRoute was not required. If the binary exists and
health fails, start loopback only **on that same host**:

```text
OMNIROUTE_SERVER_HOST=127.0.0.1 omniroute serve --port 20128 --no-open
```

Never bind a non-loopback address. Never use a remote `--base-url`.

## Live catalog on every dispatch

Do not keep a session catalog file and do not cache positive catalog rows.
Remaining tokens change after each delegate, so query live `models` and
`usage quota` immediately before every dispatch. A **user-local**
provider-exhaustion backoff cache (XDG state, never committed) may store only
negative `exhausted_until` / retry-after entries; it is not a positive catalog cache.
Update it on exhaustion signals (`state==exhausted`, `remaining<=0`, HTTP 429,
quota-reset / insufficient-balance / stream-disconnect text). Pass the failed
identity through `candidates(..., diagnostic=..., failed_identity_sha256=...,
failed_provider=...)` so the next ranking skips that entry until expiry.

```text
omniroute --output json models
omniroute --output json usage quota
omniroute --output json models <provider>
python3 chaos-engine/skills/omniroute/scripts/runner.py candidates --capability mechanical|default|most-intelligent
python3 chaos-engine/skills/omniroute/scripts/runner.py candidates --capability default --task coding|implementation|general
```

Do not add `--json` after the `models` subcommand: that form prints a table, not JSON. Use `--output json` before `models`.
Unfiltered `models` JSON is capped at 50 rows. Query `models <provider>` for each remaining quota provider so ranking is not stuck on one family.
Do not use `omniroute openapi try /api/models/catalog` without the CLI session;
it returns HTTP 401.

### Decode

1. Strip ANSI with `\\x1b\\[[0-9;]*[A-Za-z]`.
2. Find the first `{` or `[`.
3. Parse with `json.JSONDecoder().raw_decode` so trailing extra JSON is ignored.
4. Gateway `/api/models` rows use `model` (native id), `name` (display), `provider`, and `available`. CLI `omniroute models` JSON sets `id` from `name` when `id` is missing, so prefer `model` over `id`/`name`.
5. Quota is a JSON array of objects with `provider`, `remaining`, and `state`.
6. Join on alphanumeric-lowercased provider ids (`glm-cn` matches `glmcn`).
7. Drop `state == "exhausted"` or `remaining <= 0`. Also drop providers/identities still present in the user-local provider-exhaustion backoff cache before their `exhausted_until`. Do not drop `available: false`: that management flag hid models the completions live catalog still accepts. Do not drop `supportsVision: true` for implementation ranking.
8. Whitespace display names become native ids by stripping parentheticals, lowercasing, and replacing spaces with hyphens. Already-slugged ids stay unchanged. Compose `--model` as `provider/model` when the native id has no provider prefix.

### Rank (dynamic, from the live ids)

Classify each remaining `id` by its own tokens, not a stored model list:
`low|lite|flash|air|mini|nano|turbo|haiku|small` = mechanical;
`high|max|pro|ultra|opus|thinking|reasoner` = most-intelligent;
otherwise default. Architecture, review, and analytical work use only
most-intelligent. Implementation uses `default` first, then most-intelligent,
then mechanical. Do not pin a Codex profile model such as Gemini Flash-Lite.
Empty result is `RUNTIME_EXHAUSTED`.

### Coding candidate filter (implementation)

For `--capability default` and for `--task coding|implementation`, rank from
the live ids with an allow/deny token filter — never a stored model list:

- **Deny** (drop before ranking): tokens `safety`, `guard`, `translate`,
  `nano`, `tiny`.
- **Boost** (sort first within the remaining pool): tokens/substrings `code`,
  `coder`, `sonnet`, `claude`, `gpt-oss`, `qwen`, `kimi`, `devstral` (also
  ids whose tokens start with `qwen`/`kimi` or contain `code`).

Mechanical and most-intelligent capability selections without an explicit
coding task keep the older capability-only ranking and do not apply this
filter. Catalog ranking never writes route, model, or provider ids into
repository files.

When the gateway supports combo routing, prefer trying
`omniroute run --model auto/coding` (or category coding) first; on failure or
unsupported combo, fall back to the ranked native id from `candidates`.

### CLI target matrix (fail closed)

Pick target from model id (not first binary):

| Target | Compatible ids | Notes |
| --- | --- | --- |
| `claude` | Claude-family / `cc/` / `anthropic*` only | Else `ANTHROPIC_MODEL`+`EXPOSE_CC_DISCOVERY_ALIASES`, or skip `claude` (raw Kimi → `unrecognized_model`). |
| `opencode` | any coding id | `--model omniroute/<id>` once. |
| `codex` | any coding id | `-c model='<provider>/<id>'`, `wire_api=responses`, base `/v1`. |
| `qwen` / `gemini` | id verbatim | `qwen` needs `--model`. |

Exit `127` → next **target**. Rank: `claude` (Claude-family only) → `opencode` → `codex` → `qwen`/`gemini`.

### Preflight before long `omniroute run`

1. Prefer `auto/coding` / `auto/coding:fast` when live `/v1/models` advertises it and smoke (`omniroute test` / tiny `chat`) passes.
2. Else ranked native id in provider live `models <provider>`; smoke it. Fail → skip; do not start implementer.
3. `unrecognized_model` / 429 / live-catalog 400 / stream-before-completed → **0** same-identity retries; requery; next id.
4. Same identity ≤**1** retry for timeout / single network blip only.
5. 401/403 → stop OmniRoute transport.

Anti-patterns: static model allowlists as primary selector; `candidates` then native host while READY; `claude`+Kimi/Qwen without discovery aliases; broad `pkill -f` on launcher argv (see [process lifecycle](references/process-lifecycle.md)).

### Operator checklist (docs only — no credential writes)

- Claude-family connection for `claude`; OpenAI-compatible coding connection for `opencode`/`codex`.
- Leave exhausted OAuth out of default coding rank until quota recovers.
- Dashboard → Import `/models` / Auto-Sync when CLI ids drift from live `/v1`.
- Thinking Budget **passthrough**. Harness never installs OmniRoute or writes secrets.

### Dispatch checklist (READY)

1. Loopback health: `curl -sf --max-time 2 http://127.0.0.1:20128/api/health`.
2. Live `candidates` (coding filter). First remaining identity; never reuse a just-failed id on 429.
3. On `READY`, `omniroute run --model --provider <target>` before native host (target order above; skip `127`).
4. On fail: skip identity/provider, requery, next id.
5. Native host only on `RUNTIME_EXHAUSTED`, empty catalog, sealed-launcher `78`, or `ABSENT`.

**Process failure:** `candidates` then native host while READY is not success.
Catalog ≠ dispatch. When OmniRoute was required, that failure is a delivery
blocker unless [proof-of-dispatch](references/proof-of-dispatch.md) is met.
After repeated 429 / unrecognized_model thrash with no productive run, dismiss
OmniRoute with an explicit blocker or `RUNTIME_EXHAUSTED` (see
[living-lessons](references/living-lessons.md)).

Retry from failure, not a pinned profile:

- 429 / rate-limit / resource_exhausted / live-catalog 400 (`not available in the active live catalog`) / Stream closed before `response.completed`: skip identity, requery, next native id. Out-of-balance provider → skip family. Daemon `OMNIROUTE_ROTATE_ON_400=true` may hop inside gateway; ChaosEngine still skips the failed launch identity. Stream issues → Dashboard Translator.
- Timeout / single network blip: retry same pick once.
- 401/403 / invalid key: stop; fix credential (OAuth: Dashboard reconnect or `omniroute providers auth`).
- Empty remaining catalog: `RUNTIME_EXHAUSTED`, then native host.

Never pin a model in a Codex profile. Live catalog → rank → map display→native → `omniroute run`.

### Dispatch (`omniroute run`, no config writes)

Prefer `omniroute run` over `setup-*` / `configure`. `run` writes nothing.
Official run targets come from `bin/cli/cli-manifest.mjs`: `claude`
(`claude-code|cc|anthropic`), `codex` (`codex-cli|openai-codex|openai`),
`opencode` (`open-code`), `aider`, `goose` (`goose-cli`), `qwen`
(`qwen-code`; `--model` required), `gemini` (`gemini-cli`). Missing binary
exits `127`: skip that target. Invalid args exit `2`. Child exit is
propagated. Do not use `setup-codex`, `setup-claude`, or `setup-opencode`
from a task. Do not pass `--remote` or a non-loopback `--base-url`. Do not
dispatch through OmniRoute Chaos Mode (`/dashboard/chaos` or `auto/chaos`).

`--model` wiring (CLI-INTEGRATIONS):

- **claude**: `ANTHROPIC_MODEL`. `ANTHROPIC_BASE_URL` is the gateway **root,
  no `/v1`**. Pin non-Claude ids with `--model`; the `/model` picker lists
  only `claude*`/`anthropic*` unless `EXPOSE_CC_DISCOVERY_ALIASES`.
- **opencode**: `--model omniroute/<id>` (prefix added only if missing).
- **qwen** / **gemini**: id verbatim. Gemini uses root (`/v1beta`).
- **goose**: `GOOSE_MODEL`. Base URL is root.
- **codex**: `-c model_providers.omniroute.*` with `base_url` **including
  `/v1`** and `wire_api=responses` (never `chat`). For Codex, pass Codex `-c model='<provider>/<id>'` after `--`; `omniroute run` sets
  `model_provider=omniroute` but leaves Codex's default model name.

Pick the first installed implementer target: `claude`, then `opencode`, then
`codex`. Launch from the delegate worktree as cwd.

```text
omniroute run --port 20128 --model '<id>' --provider '<provider>' claude -- --print --dangerously-skip-permissions '<prompt>'
omniroute run --port 20128 --model '<id>' --provider '<provider>' opencode -- run --auto --dir '<worktree>' '<prompt>'
omniroute run --port 20128 --model '<id>' --provider '<provider>' codex -- -c model='<provider>/<id>' exec --ephemeral --approve-for-me -C '<worktree>' '<prompt>'
```

Local Codex `env_key` accepts placeholder `OMNIROUTE_API_KEY=local` when the
gateway is unauthenticated; a real inference key is required for protected
`/v1`. GET `/v1/models` without that key returns 401; that is not a dispatch
failure. Thinking Budget on the OmniRoute host must be `passthrough` or
client effort/summary is stripped. Long tasks: raise `sessionAffinityTtlMs`
above expected wall-clock; set `STREAM_IDLE_TIMEOUT_MS=0` and
`FETCH_BODY_TIMEOUT_MS=0` (or above the longest quiet gap). Heartbeats do
not reset the idle clock.

Then follow [orchestrator follow-through](../../references/orchestrator-follow-through.md)
until the delegate exits with closing notes. Rank free/remaining catalog
entries first. If those fail, use any other model the local endpoint can call.
Native host models only when OmniRoute itself cannot run.

Canonical orchestration must probe the fixed loopback endpoint before native
fallback, with no endpoint prompt. On `READY` after a live `candidates` pick,
**must** dispatch through `omniroute run` as above before any native host
model. A concrete `RUNTIME_EXHAUSTED` health result, empty remaining catalog,
missing OmniRoute binary, or sealed-launcher exit code `78` permits native
implementer fallback.

## Runner

Use only the standard-library [runner](scripts/runner.py):

```text
python3 chaos-engine/skills/omniroute/scripts/runner.py probe
python3 chaos-engine/skills/omniroute/scripts/runner.py candidates --capability mechanical|default|most-intelligent
python3 chaos-engine/skills/omniroute/scripts/runner.py candidates --capability default --task coding
python3 chaos-engine/skills/omniroute/scripts/runner.py proof --required --receipt <private-state>/receipt.json
python3 chaos-engine/skills/omniroute/scripts/runner.py dispatch --contract <private-state>/dispatch.json
python3 chaos-engine/skills/omniroute/scripts/runner.py status ...
python3 chaos-engine/skills/omniroute/scripts/runner.py cancel ...
python3 chaos-engine/skills/omniroute/scripts/runner.py complete --contract <private-state>/complete.json
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

## Delegate continuity

Opt-in bounded continuity (capability floor, alternates, `_supervise`) lives in
[references/delegate-continuity.md](references/delegate-continuity.md). Omit
`continuity` for unchanged legacy one-shot dispatch.
