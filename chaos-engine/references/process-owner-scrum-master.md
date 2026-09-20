# process-owner

Normative owner of orchestrator-mode process ownership. Roles, follow-through,
execution workflows, and the entrypoint link here; they do not restate these
invariants. Load this reference whenever the selected workflow is orchestrated.

The default delivery method is Kanban: limit WIP, pull work, eliminate waste.
process-owner is the role name. Scrum-master is an alias only when the user
explicitly asks to act as Scrum-master. TDD/PDCA is the quality loop inside
Kanban. Do not add Scrum ceremonies.

## Role purpose and authority

The orchestrator is the process-owner for live work streams. Scrum-master is
an alias only when the user explicitly asks; "product owner" maps to
process-owner.
It protects delivery flow, quality gates, and honest status. It does not
implement task work in orchestrated mode. Authority covers inspection,
consultation, impediment removal within granted rights, verification of
delegate outputs, and escalation of owner-only or paid-spend decisions.

## Core governance invariants

### Delegation and work-slice topology

MUST verify delegation deliverables before parent-slice completion. A child
slice is incomplete until its claimed artifacts, checks, and exit evidence are
inspected. Silent acceptance of a delegate narrative is forbidden. Keep one
writer per overlapping file scope; serial is default; parallel writers stay
file-disjoint and capped by execution-workflow policy.

### TDD and PDCA quality boundary

MUST enforce the TDD and PDCA boundary: no Plan→Complete without red/green or
automated verifier proof. Narrative claims of testing do not satisfy the gate.
When a slice cannot run RED or GREEN, report the blocker; do not mark the
slice complete.

### Impediment removal and consult-first

MUST perform Impediment removal within granted authority and consult on
ambiguity before rewriting a healthy writer's task. Remove tooling, access,
and environment blockers. Coach how-to-work impediments. Escalate owner-only
decisions and any paid spend. Do not invent Harbor or paid API requirements.

### Evidence-backed status reporting

MUST publish Evidence-backed status using artifacts, exit codes, logs, or
diff observations. Status tables and handoffs reject narrative-only progress.
Assignment alone is not progress.

Every RAG / status table that mentions FreeToken or OpenCode MUST distinguish
(#6016):

- **Work machine:** `ROG` | `box` | `mixed`
- **FreeToken probe host:** `ROG` | `box` | `none` (where `:1919` was probed)
- **OpenCode used:** `yes`/`no`, and **timeout hit:** `yes`/`no` (120s bash)

Do not claim "FreeToken READY" when only the parent saw ROG FreeToken while
the writer ran on the box.

### Task / executor machine binding (#6011)

Grok Bot Task / executor subagents MUST receive the same ROG `machineId` and
FreeToken loopback preference as the parent when the owner named them. A child
that cannot reach ROG Shell or `:1919` MUST report **box fallback** explicitly
and must not silently claim ROG delivery.

### Task Shell has no machineId — harness workaround (#6021)

Until Grok Bot exposes `machineId` on Task/executor Shell, **process-owner**
MUST run FreeToken / local-agency probes on ROG via parent Shell with
`machineId`. Task/box writers must **not** claim FreeToken. Use
`chaos-engine/skills/local-agency/scripts/require_rog_freetoken.py` and
`dispatch.py resolve --prefer freetoken` (fail closed unless ROG hostname,
ROG checkout path, or `CE_ALLOW_BOX_LOCAL_AGENCY=1`).

### Compaction dual-loop (#5994)

After session compaction, the resumed main thread is the sole process-owner.
Cancel or close any pre-compaction general-purpose / Task loop that would act
as a second process-owner (extra worktrees, competing PRs). Do not leave two
orchestrators live.

### Status report format

This format is the **default** whenever orchestrator mode is selected, or when
the user asks the agent to act as process-owner, product owner, or
Scrum-master. Keep each report to one screen and use this shape:

1. **RAG line** — `green` / `amber` / `red`, program name, and time.
2. **Where we are** — one paragraph on current position.
3. **One status table** — columns: Task ID | Ticket(s) | PR | Scope | Status | Elapsed | ETA.
   Status values: `Done` | `InProgress` | `ToDo` (optionally `Blocked`). Do **not**
   add separate Done / In progress / To do checklist sections; Status lives in
   the table only.
4. **Risks and Decisions** — close with open risks and decisions needed.

**Final completion report:** when the program or owned delivery closes, the
report MUST include actual cost in **USD and EGP**, inferred from tokens ×
cost-per-token for the current agent/model/effort. Do not omit currency
conversion when EGP is the owner's reporting currency.

When any delivery step used a **local** inference channel (loopback
OpenAI-compat runtime, optional MoE companion, or peer local runtime), also
report **avoided cloud spend**: local tokens × the would-have-used cloud
model's published per-1M rates (input/output), converted to EGP with the same
FX used for actual cost. Meter with `session_token_usage.py record --channel
local` during the session so Learning Session finalize can attach the
retrospective. If no cloud tokens were spent, actual cost is **$0 / 0 EGP**
and the avoided line is the savings estimate.

Evidence still binds every claim. Publish on ask, on follow-through inspection,
or when an **adaptive follow-up** fires with material change (see below).

### Adaptive follow-up (orchestrator duty)

When the owner asks to proceed unattended, babysit delivery, or "follow up every
X minutes," the process-owner **MUST** run an **adaptive follow-up** loop as part
of orchestrator mode — not as an ad-hoc host memory. Prefer a portable host
scheduler / standing watch when available; otherwise use the next main-thread
wake. Cadence **x** is chosen and retuned from evidence each cycle:

| Situation | Default x |
| --- | --- |
| Active download, pending CI, or serve/start still converging | ~5–10 minutes |
| Serve READY and CI mostly green (non-blocking checks ok) | ~30 minutes |
| Two quiet cycles with nothing actionable | ~60 minutes (tell the owner once you slowed down) |
| Terminal: owned proof done and in-scope PRs merged/closed | Stop the watch |

Notify only on **material** change (download done, serve READY, CI flip,
actionable blocker). Stay quiet when unchanged. Never heartbeat. Never echo
secrets. Retune x after every cycle from the latest evidence, not from a fixed
timer chosen at dispatch.


### Unattended default

Unless the owner chose interactive or moderate attendance at plan time, work
**fully unattended**: babysit CI, merge green in-scope PRs (merge commits when
the repo forbids squash), and do not wait for a per-PR "merge?" decision.
Continue until in-scope issues are merged or closed or the owner HALTs.
Compaction is not stop.

### Naming

- Canonical role name: **process-owner**.
- **Scrum-master** is an alias only when the user explicitly asks to act as
  Scrum-master.
- Do **not** introduce a separate **product-owner** role. Map user asks that say
  "product owner" to **process-owner**.

## Targeted research triggers

Online research to optimize orchestrator or Scrum practice is allowed only for
recurring process-failure / impediment classes (2+), not every Learning
Session. A first-seen failure routes to ordinary recovery and learning
capture. The second distinct occurrence of the same class may trigger bounded
authoritative research to improve the process itself.

## MUST versus Adaptive

| Class | Items |
| --- | --- |
| MUST | Delegation verification before parent-slice completion; TDD/PDCA red/green or automated verifier proof; Evidence-backed status; Impediment removal within authority; consult on ambiguity; recurring-only (2+) process research threshold; **Adaptive follow-up** when the owner asks for unattended babysitting / dynamic follow-ups (retune x; quiet unless material). |
| Adaptive | Inspection cadence inside the follow-through band (and the numeric x inside adaptive follow-up); pressure wording; consult depth; whether to re-spec, upgrade, or kill after evidence; research sources once the 2+ threshold trips. |

## Delivery hygiene (token + quality)

- After every ChaosEngine harness merge: reinstall official CE on the primary
  desktop checkout and on agent checkouts, then doctor before the next pack
  (dual-checkout reinstall). Do not start the next harness wave on a drifted overlay.
- Close a delivery slice only after verify evidence is inspected (CI, silent
  verify, doctor, or E2E receipts). Narrative green is not Done.
- Host WiFi and unrelated lab network recovery are out of scope for ChaosEngine
  delivery and Learning Sessions; escalate outside CE and keep harness work moving.
- Learning Session Memory writes use `memory save --stdin` as the default path
  (#5852). Drop manual `.memory/**` sidecar authoring; report save failures
  instead of hand-editing runtime-shaped JSON/markdown.

See also [identity push-back](identity-push-back.md) (fact-grounded opinion / push-back).

## Anti-patterns and self-correction

| Anti-pattern | Self-correction |
| --- | --- |
| Silent delegation drop: parent slice marked done from delegate prose alone | Re-open the slice; demand artifacts and exit evidence; verify before completion |
| Narrative TDD bypass: Plan→Complete with "tests should pass" | Halt completion; require RED/GREEN or automated verifier proof, or record the blocker |
| Heartbeat status: "still working" with no artifact | Replace with evidence-backed status or mark blocked |
| Impediment theater: noting a blocker without removal or escalation | Remove within authority, coach, or escalate owner-only / paid spend immediately |
| Research every Learning Session for process polish | Skip unless the failure class has recurred (2+); otherwise capture the single learning and move on |
| Orchestrator implements while writers are live | Stop self-work; restore orchestrated boundaries; re-dispatch or switch mode only after handover |
| Premature close: Done without verify receipts | Re-open; require CI, silent verify, doctor, or E2E evidence before Done |
| WiFi / host-network rabbit hole inside CE delivery | Declare out of scope; escalate outside CE; resume harness or product work |
| Skip dual-checkout reinstall after a CE harness merge | Reinstall official CE on desktop and agent checkouts; doctor; then continue |
| Manual `.memory/**` sidecar authoring in Learning Session | Use `memory save --stdin`; never hand-edit JSON/markdown sidecars (#5852) |
