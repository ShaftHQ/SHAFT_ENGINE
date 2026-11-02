# ChaosEngine decision-quality baseline

Accessed: 2026-09-01. Evidence sourced exclusively from public GitHub issue
and PR history for ShaftHQ/SHAFT_ENGINE. No prompts, transcripts, model IDs,
provider routes, endpoint URLs, private paths, or session-level telemetry
included. All provider and model references replaced with low-cardinality
labels.

Parent tracking epic: #5514. Deliverable for #5520. This artifact supplies
the shared metrics dictionary and taxonomy that #5521–#5525 import by
reference.

---

## Metrics dictionary

Missing telemetry is recorded as `UNAVAILABLE`. It is never substituted with
zero, an estimate, or a placeholder value.

| Metric | Definition | Missing-data policy |
| --- | --- | --- |
| `total_tokens` | Accumulated input + output tokens for the session or sub-task | `UNAVAILABLE` when telemetry not collected |
| `wall_time_minutes` | Elapsed real-world minutes from task start to merged PR; approximated from commit/PR timestamps when exact telemetry unavailable | `UNAVAILABLE` when neither telemetry nor public timestamps exist |
| `external_run_minutes` | Minutes of external process invocations (CI dispatches, workflow runs) that blocked forward progress | `UNAVAILABLE` when unobservable from public evidence |
| `retry_count` | Number of repeated invocations of the same tool, workflow, or call within one task before the root owner was fixed | Counted from public comment/commit evidence; `UNAVAILABLE` when evidence absent |
| `fix_iterations` | Number of distinct fix commits addressing the same root owner before the closing commit | Counted per public commit SHA sequence |
| `changed_files` | Files modified in the PR diff | From `gh pr diff --stat`; `UNAVAILABLE` when PR not yet open |
| `pr_count` | Number of PRs opened to deliver the task | From public GitHub PR history |
| `reopened_failures` | Count of failures re-encountered after a partial fix was merged | From orchestration status comments; `UNAVAILABLE` when not publicly documented |
| `escaped_defects` | Defects that passed CI but failed in a downstream task or follow-on issue | From failure comments in downstream issues; `UNAVAILABLE` when not traceable |

---

## Sampling protocol

Reproducible from public GitHub commit and comment evidence only.

1. Identify closed or in-progress sub-issues of a tracking epic (e.g., children of #5514).
2. For each sub-issue: collect commit SHAs from orchestration status comments and PR merge records in the public thread.
3. Count `fix_iterations` = number of distinct commits referencing the same defect class before the closing commit.
4. Count `retry_count` from repeated failure mentions in issue comments (e.g., "retry reason:", numbered status rows with the same ID appearing in multiple status updates).
5. Classify each instance using the taxonomy below; one instance may match multiple classes.
6. **Privacy gate:** before recording any sample, strip all model IDs, provider names, endpoint URLs, credential references, and private paths. Replace with labels: `[PROVIDER]`, `[MODEL_ID]`, `[ENDPOINT]`. A sample that cannot be fully redacted is discarded.
7. **Validity check:** a sample is accepted iff all three conditions hold:
   - At least one public commit SHA exists as a causal anchor.
   - At least one error code class or failure description is present in a public comment (not inferred).
   - No prompt content, session transcript, or private path is present.

---

## Quantified taxonomy

### Class 1: Symptom-fix before root owner (`SYM-BEFORE-ROOT`)

**Definition:** One or more fix commits address a presenting error without
identifying the invariant owner of the failure. Subsequent transport attempts
hit the next downstream error in the same causal chain.

**Causal mechanism:** Each iteration eliminates the most recent error signal
(HTTP status code, rejection message) without tracing the call path to the
architectural boundary that owns the contract. The loop continues until a
reframe forces boundary identification.

**Public evidence (ShaftHQ/SHAFT_ENGINE, epic #5514):**
- fix_iterations = 3 observable before root-cause resolution
- Commits (public, redacted of routing detail): three catalog-ranking patches
  each resolved the presenting error; a fourth patch collapsed remaining
  transport failures by naming the correct catalog boundary
- retry_count ≥ 4 transport retries on the same task before root-cause fix
- reopened_failures = 1 (same failure class re-emerged after first patch)
- total_tokens: `UNAVAILABLE`
- wall_time_minutes: `UNAVAILABLE` (multiple sessions)

**Limitations:**
- Low-cardinality count from one public task; not statistically generalizable.
- Evidence quality: plausible-confirmed via public commit sequence and
  orchestration status comments.
- Agent-vs-human contribution to root-cause identification not
  distinguishable from public evidence alone.

**Rollback rule:** if this class is removed from the taxonomy, revert by
restoring the class definition and its anchoring commit SHAs. The causal
anchor is the first orchestration status comment noting a failure in the
same error family.

**Retention rule:** retain while sibling issues (#5521–#5525) have not yet
confirmed or refuted with an independent instance. Promote to permanent once
one independent instance is confirmed.

---

### Class 2: Stale or low-information repeated retrieval (`STALE-RETRIEVAL`)

**Definition:** The same store, catalog, or data source is re-queried across
fix iterations when its contents have not changed, adding latency and token
cost with no new information.

**Causal mechanism:** No explicit staleness gate or stopping rule on
retrieval; re-query is the default action on any failure even when the failure
source is not the queried store. The absence of new information is not
detected; the loop continues.

**Public evidence (ShaftHQ/SHAFT_ENGINE, epic #5514):**
- Three successive catalog-ranking patches each required re-reading the model
  catalog; catalog availability did not change between reads
- retry_count per class instance: ≥ 3 catalog reads, 0 new information units
  per re-read (catalog contents stable across reads per public comment evidence)
- fix_iterations attributable to this class: 2 (patches 1 and 2 addressed
  catalog structure, not catalog staleness; the re-read was avoidable)
- total_tokens: `UNAVAILABLE`
- external_run_minutes: `UNAVAILABLE`

**Limitations:**
- Token cost is unobservable from public evidence; marked `UNAVAILABLE`.
- The catalog re-read count is inferred from the commit sequence; direct
  tool-call counts are not public.
- Evidence quality: plausible from public comment pattern and commit sequence.

**Rollback rule:** remove class if a confirmed stopping-rule implementation
shows the pattern no longer occurs in a subsequent task. The rollback
anchor is the issue or PR that documents the stopping-rule addition.

**Retention rule:** retain while no explicit staleness gate exists in the
harness guidance. Retire to archive (do not delete) once a stopping rule
is adopted and confirmed effective by at least one task.

---

### Class 3: Late architectural boundary identification (`LATE-ARCH`)

**Definition:** An architectural boundary — a distinct API surface, ownership
domain, or availability contract — is not named until after multiple
symptom-fix iterations. Once named, it collapses the remaining failures into
one targeted fix.

**Causal mechanism:** Implementation-level error messages (HTTP status codes,
rejection text) are treated as root cause rather than as signals pointing to
a boundary mismatch. The boundary is implicit in the system design but not
surfaced in the agent's working model. A reframe (by human or by the agent
itself) names the boundary explicitly.

**Public evidence (ShaftHQ/SHAFT_ENGINE, epic #5514):**
- Architectural boundary: two distinct catalog APIs with different
  availability semantics (management catalog vs. completions catalog)
- fix_iterations before boundary named: 4
- fix_iterations after boundary named: 1 (single patch resolved remaining
  transport failures)
- retry_count before boundary named: ≥ 4 transport failures
- Reframe source: human comment in orchestration status thread
- pr_count for root-cause fix: 1
- reopened_failures after boundary named: 0 (no further transport failures
  in same class)
- total_tokens: `UNAVAILABLE`
- wall_time_minutes: `UNAVAILABLE`

**Limitations:**
- Reframe was human-initiated; whether an agent-initiated reframe would have
  occurred at the same point is not determinable from public evidence.
- One instance from one task; not generalizable without additional samples.
- Evidence quality: confirmed via public commit sequence and comment thread.

**Rollback rule:** the boundary name (`management catalog` vs. `completions
catalog`) is a durable factual record; rollback means removing it from the
taxonomy if the boundary is later collapsed or renamed in the product. The
anchoring SHA is the commit that first named the split in code.

**Retention rule:** retain permanently once the architectural boundary name
appears in a test invariant or in harness guidance. Boundary names are
low-decay knowledge and do not require re-validation across tasks.

---

## Privacy constraints (normative)

The following are never recorded in this artifact or in any sample:

- Prompt content or session transcripts
- Model IDs or provider names (replaced with `[MODEL_ID]`, `[PROVIDER]`)
- Endpoint URLs or gateway routes (replaced with `[ENDPOINT]`)
- Credential references or secret material
- Private file paths or machine-local state
- Runtime indexes or session-level telemetry

A redaction failure invalidates the sample. Partial redaction is not
permitted; full redaction or full discard.

---

## Reuse contract for #5521–#5525

Each sibling issue may:
- Add instances to existing classes using the same evidence format.
- Promote a class from `plausible` to `confirmed` once a second independent
  instance is recorded.
- Add a new class if a pattern not covered here recurs with at least one
  causal anchor.

Each sibling issue must not:
- Modify the metrics dictionary without updating this file and its test.
- Remove a class without documenting the removal rationale and rollback anchor.
- Record an instance that violates the sampling protocol privacy gate.
